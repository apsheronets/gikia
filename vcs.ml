(* This file is part of gikia.
 *
 * gikia is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * gikia is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with gikia.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright 2010-2012 Alexander Markov *)

open Lwt
open ExtLib
open Printf
open Utils
open CalendarLib
open Simplexmlparser

let timeout = Io.timeout

type date =
  | Calendar of Calendar.t
  | Rfc of string (* rfc 2828 *)

type change = {
  author: string;
  date:   date;
  hash:   string;
  title:  string;
  comment: string list;
}

(** relative path *)
type r_path = string list

let r_path_of_string s =
  String.nsplit s "/" >>
  dtfilter

type file_status =
  | MovedTo of r_path
  | Removed

let open_del  = "{-"
let close_del = "-}"
let open_ins  = "{+"
let close_ins = "+}"
let wdiff_cmd =
  sprintf "wdiff -d -w '%s' -x '%s' -y '%s' -z '%s'"
    open_del close_del open_ins close_ins

let wdiff str =
  (* TODO: add debug here *)
  let command = Lwt_process.shell wdiff_cmd in
  let p = Lwt_process.open_process_full ~timeout command in
  Lwt_io.write p#stdin str >>= fun () ->
  Lwt_io.close p#stdin  >>= fun () ->
  Lwt_io.read  p#stdout >>= fun out ->
  Lwt_io.read  p#stderr >>= fun err ->
  p#close >>= fun _ ->
  if String.length err = 0
  then return out
  else fail (Io.Error (sprintf "%s: %s" wdiff_cmd err))

module type VCS =
sig
  exception Error of string
  val get_changes : ?first:int -> ?count:int ->
    ?path:string -> string -> change list Lwt.t
  val get_diff : string -> string -> string -> string Lwt.t
  val get_full_diff: string -> string -> string Lwt.t
  val get_wdiff : string -> string -> string -> string Lwt.t
  val where_file_leaves : string -> string -> string list -> (file_status option) Lwt.t
end

module Darcs : VCS =
struct
  exception Error of string

  let unescape s =
    let s = replace s "&amp;" "&" in
    let s = replace s "&lt;" "<" in
    let s = replace s "&gt;" ">" in
    let s = replace s "&quot;" "\"" in
    s

  let to_calendar s =
    Scanf.sscanf s "%4u%2u%2u%2u%2u%2u"
      (fun y m d h min s ->
        Calendar.make y m d h min s)

  let get_changes ?first ?count ?path repodir =
    let cmd =
      let range =
        match count with
        | Some c -> [| sprintf "--max-count=%u" c |]
        | None   -> [| |] in
      let args =
        Array.concat [
          [|"darcs"; "changes"; "--quiet"; "--xml-output"|];
          range;
          [|"--repodir="^repodir|];
          match path with Some p -> [|p|] | None -> [||] ] in
      ("darcs", args) in
    catch (fun () ->
      Io.exec cmd >|= fun str ->
      let attrib attrs name =
        let _, v = List.find (fun (n, _) -> n = name) attrs in
        v in
      match Simplexmlparser.xmlparser_string str with
      | [ Element ("changelog", _, l) ] ->
          List.fold_right (fun x acc ->
            match x with
            | Element ("patch", attrs, info) ->
                let author = attrib attrs "author"
                and date   = attrib attrs "date"
                and hash   = attrib attrs "hash"
                and title = exude (function
                  | Element ("name", _, [PCData s]) -> Some s
                  | _ -> None) info
                and comment =
                  exude (function
                    | Element ("comment", _, l) -> Some l
                    | _ -> None) info >>
                  List.map (function
                    | PCData s -> s
                    | _ -> raise (Error
                        "unexpected element inside comment")) >>
                  List.remove_if
                    (fun x -> String.starts_with x "Ignore-this:")
                in
                { author  = unescape author;
                  date    = Calendar (to_calendar (unescape date));
                  hash    = unescape hash;
                  title   = unescape title;
                  comment = List.map unescape comment;
                } :: acc
            | _ -> acc) l []
      | _ -> raise (Error "incorrect xml in darcs output"))
    (function
      | e -> fail (Error (Printexc.to_string e))) >>= fun changes ->
    match first with
    | Some f -> return (List.drop (f-1) changes)
    | None   -> return changes

  let get_diff repodir page hash =
    let cmd = ("darcs", [|"darcs"; "diff"; "--store-in-memory"; "--quiet"; "-u";
      "--repodir="^repodir; "--match=hash "^hash; page|]) in
    Io.exec cmd

  let get_full_diff repodir hash =
    let cmd = ("darcs", [|"darcs"; "diff"; "--store-in-memory"; "--quiet"; "-u";
      "--repodir="^repodir; "--match=hash "^hash|]) in
    Io.exec cmd

  let get_wdiff repodir page hash =
    let sh = sprintf "darcs diff --store-in-memory -u --quiet '--repodir=%s' '--match=hash %s' '%s' | %s"
      (quote repodir) (quote hash) (quote page) wdiff_cmd in
    let cmd = Lwt_process.shell sh in
    Io.exec cmd

  type summary_file_status =
    | Move of (r_path * r_path)
    | Modify_file of (r_path * (int * int))
    | Add_file         of r_path
    | Remove_file      of r_path
    | Add_directory    of r_path
    | Remove_directory of r_path

  let get_summary repodir path =
    let cmd =
      let args =
        [|"darcs"; "changes"; "--quiet"; "--summary"; "--xml-output";
          "--repodir="^repodir; path|] in
      ("darcs", args) in
    catch (fun () ->
      Io.exec cmd >|= fun str ->
      let process_summary =
        let r_path_of_string s =
          r_path_of_string (String.strip (unescape s)) in
        List.map (function
          | Element ("move", [("from", move_from); ("to", move_to)], _) ->
              Move (r_path_of_string (unescape move_from), r_path_of_string (unescape move_to))
          | Element ("modify_file", _, child) ->
              Modify_file (List.fold_left (fun acc x ->
                match acc, x with
                | ([], (added, removed)), PCData s ->
                    (r_path_of_string s), (added, removed)
                | (filename, (0, removed)), Element ("added_lines", [("num", n)], _) ->
                    filename, ((try int_of_string n with _ -> assert false), removed)
                | (filename, (added, 0)), Element ("removed_lines", [("num", n)], _) ->
                    filename, (added, (try int_of_string n with _ -> assert false))
                | _ -> assert false) ([], (0, 0)) child)
          | Element ("add_file",    _, [PCData file]) ->
              Add_file    (r_path_of_string file)
          | Element ("remove_file", _, [PCData file]) ->
              Remove_file (r_path_of_string file)
          | Element ("add_directory",    _, [PCData dir]) ->
              Add_directory    (r_path_of_string dir)
          | Element ("remove_directory", _, [PCData dir]) ->
              Remove_directory (r_path_of_string dir)
          | Element (s, _, _) -> failwith s
          | _ -> raise (Error "incorrect xml in darcs summary")) in
      match Simplexmlparser.xmlparser_string str with
      | [ Element ("changelog", _, l) ] ->
          List.fold_right (fun x acc ->
            match x with
            | Element ("patch", attrs, info) ->
                let summary =
                   exude (function
                     | Element ("summary", _, l) -> Some l
                     | _ -> None) info >>
                   process_summary in
                summary :: acc
            | _ -> acc) l []
      | _ -> raise (Error "incorrect xml in darcs output"))
    (function
      | e ->
        fail (Error (Io.string_of_cmd cmd ^ ": " ^ Printexc.to_string e)))

  let where_file_leaves repodir _a _r =
    let rec loop = function
      | Remove_file f :: _ when f = _r ->
          Some (Removed)
      | Remove_directory d :: _ when d = _r ->
          Some (Removed)
      | Move (src, dst) :: _ when src = _r ->
          Some (MovedTo dst)
      | _::t -> loop t
      | [] -> None in
    get_summary repodir _a >>= fun summary ->
    return (loop (List.flatten summary))

end

module Git : VCS =
struct
  exception Error of string

  let get_changes ?first ?count ?path repodir =

    let sh =
      let range =
        match first, count with
        | Some f, Some c ->
            sprintf "--skip=%u -n %u" (f-1) c
        | Some f, None ->
            sprintf "--skip=%u" f
        | None, Some c ->
            sprintf "-n=%u" c
        | None, None ->
            "" in
      let path =
        match path with
        | Some p -> sprintf "-- '%s'" (quote p)
        | None -> "" in
      sprintf "cd '%s' && git log --date=rfc %s %s"
        (quote repodir) range path in
    catch (fun () ->
      let cmd = Lwt_process.shell sh in
      let lines = Lwt_process.pread_lines ~timeout cmd in
      let get_change lines =
        let get str s =
          if String.starts_with str s
          then String.slice str ~first:(String.length s)
          else raise (Error
            (sprintf "Expected %S, recived %S" s str)) in
        Lwt_stream.next lines >>= fun hash ->
        Lwt_stream.junk_while
          (fun s -> String.starts_with s "Merge: ") lines >>= fun () ->
        Lwt_stream.next lines >>= fun author ->
        Lwt_stream.next lines >>= fun date ->
        Lwt_stream.next lines >>= function
        | "" ->
          Lwt_stream.get_while
            (fun s -> String.starts_with s "    ") lines >>= fun descr ->
          let l = List.map (fun s -> String.slice s ~first:4) descr in
          let title, comment = try hdtl l
            with Failure _ -> raise (Error "no description of commit") in
          return { author = get author "Author: ";
                   date = Rfc (get date "Date: ");
                   hash = get hash "commit ";
                   title = title;
                   comment = comment;
                 }
        | s -> fail (Error
          (sprintf "Expected empty line, reviced %S" s)) in
      let rec loop acc =
        catch (fun () ->
          Lwt_stream.junk_while ((=) "") lines >>= fun () ->
          get_change lines >>= fun c ->
          loop (c::acc))
          (function
            | Lwt_stream.Empty -> return (List.rev acc)
            | e -> fail e) in
      loop [])
    (function
      | e -> fail (Error (Printexc.to_string e)))

  let get_diff repodir page hash =
    let sh = sprintf "cd '%s' && git show '%s' -- '%s'"
      (quote repodir) (quote hash) (quote page) in
    let cmd = Lwt_process.shell sh in
    Io.exec cmd

  let get_full_diff repodir hash =
    let sh = sprintf "cd '%s' && git show '%s'"
      (quote repodir) (quote hash) in
    let cmd = Lwt_process.shell sh in
    Io.exec cmd

  let get_wdiff repodir page hash =
    let sh = sprintf "cd '%s' && git show '%s' -- '%s' | %s"
      (quote repodir) (quote hash) (quote page) wdiff_cmd in
    let cmd = Lwt_process.shell sh in
    Io.exec cmd

  (** not implemented, always None *)
  let where_file_leaves _ _ _ =
    return None

end

type vcs = Darcs | Git

(* Returns used VCS *)
let vcs repodir =
  Io.file_exists (repodir ^/ "_darcs") >>= function true -> return Darcs | false ->
  Io.file_exists (repodir ^/ ".git") >>= function true -> return Git | false ->
  fail (Failure (sprintf "can't detect any VCS in %S" repodir))

let get_changes ?first ?count ?path repodir =
  vcs repodir >>= function
  | Darcs -> Darcs.get_changes ?first ?count ?path repodir
  | Git   -> Git.get_changes   ?first ?count ?path repodir

let get_diff repodir page hash =
  vcs repodir >>= function
  | Darcs -> Darcs.get_diff repodir page hash
  | Git   -> Git.get_diff repodir page hash

let get_full_diff repodir hash =
  vcs repodir >>= function
  | Darcs -> Darcs.get_full_diff repodir hash
  | Git   -> Git.get_full_diff repodir hash

let get_wdiff repodir page hash =
  vcs repodir >>= function
  | Darcs -> Darcs.get_wdiff repodir page hash
  | Git   -> Git.get_wdiff repodir page hash

let where_file_leaves repodir _a _r =
  vcs repodir >>= function
  | Darcs -> Darcs.where_file_leaves repodir _a _r
  | Git   ->   Git.where_file_leaves repodir _a _r

open Parsercomb
open ExtLib

let markup_changes str =
  let str = esc str in
  let str = replace str "{+" "<ins>"  in
  let str = replace str "+}" "</ins>" in
  let str = replace str "{-" "<del>"  in
  let str = replace str "-}" "</del>" in
  str

let xhtml_of_wdiff str =
  (* remove timestamp *)
  let str =
    let cut =
      let diff_cmd =
        let p_eol = p_pred (fun c -> c = '\n') in
        (* example: *)
        p_str "{---" >>> p_str_until p_eol >>>
        p_str "{+++" >>> p_str_until p_eol >>>
        return () in
      p_str_until_p diff_cmd >>= fun (before, _) ->
      p_rest >>= fun rest ->
      return (before ^ rest) in
    match cut (str, 0) with
    | Parsed (r, _) -> r
    | Failed -> str in
  markup_changes str

let xhtml_of_full_wdiff s =
  let markup_diff =
    let diff_cmd =
      let p_ws = p_pred (fun c -> c = ' ' || c = '\t' || c = '\n') in
      let p_eol = p_pred (fun c -> c = '\n') in
      let p_wss = p_many p_ws in
      (* example: *)
      (* diff -rN -u old-k.b.n/ru/генерация-биткоинов new-k.b.n/ru/генерация-биткоинов *)
      p_str "diff" >>> p_wss >>>
      p_many (p_char '-' >>> p_str_until p_ws) >>> p_wss >>>
      p_str_until p_ws >>= fun path1 ->
      p_wss >>>
      p_str_until p_ws >>= fun path2 ->
      p_str "{---" >>> p_str_until p_eol >>>
      p_str "{+++" >>> p_str_until p_eol >>>
      return path2 in
    let diffs =
      let end_of_diff =
        (diff_cmd >>= fun file -> return (Some file))
        ||| (p_end >>> return None) in
      let rec loop acc file =
        p_str_until_p end_of_diff >>= function
        | diff, None -> return (List.rev ((file, diff)::acc))
        | diff, Some next_file -> loop ((file, diff)::acc) next_file in
      diff_cmd >>= fun file ->
      loop [] file in
    p_str_until_p diffs >>= fun (prelude, diffs) ->
    let diffs =
      List.fold_left
        (fun acc (file, diff) ->
          let path = String.nsplit file "/" in
          (* remove old-...|new-... crap *)
          let page_p =
            match path with
            | _::t -> t
            | _ -> assert false in
          let page = String.concat "/" page_p in
          let link = "<a href=\"" ^ esc (params_to_string page_p) ^ "\">" ^ esc page ^ "</a>" in
          acc ^ "\n" ^ link ^ "\n" ^ (markup_changes diff))
        ""
        diffs in
    return (esc prelude ^ diffs) in
  match markup_diff (s, 0) with
  | Parsed (r, _) -> r
  | Failed -> esc s
      (* FIXME: add error to log *)

