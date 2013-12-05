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
 * Copyright 2010-2013 Alexander Markov *)

open Lwt;
open Printf;
open ExtLib;

open Utils;
open Init;

(*type request =
  { hostname: string
  ; p: string list
};*)

value mainfeed_title hostname = sprintf "Recent changes to %s" hostname;

open Views;
open Routes;

(* Makes an article *)
value render_article hostname _a _p =
  Markup.get_page _a >>= fun (title, content) ->
  let title =
    match title with
    [ Some title -> title
    | None -> params_to_string _p ] in
  let links = link_to_atom _p in
  let a =
    object
      method title = title;
      method content = content;
      method links = links;
      method _p = _p;
      method hostname = hostname;
    end in
  render_with_layout Article.f a;

value render_full_history hostname =
  let title = sprintf "Full history of %s" hostname in
  Vcs.get_changes prefix >>= fun changes ->
  let a =
    object
      method title = title;
      method changes = changes;
      method hostname = hostname;
      method links = "";
    end in
  render_with_layout Full_history.f a;

value render_history hostname _p =
  let _a =
    match _p with
    [ [] -> None
    | _p -> Some (get_path prefix _p) ] in
  let title = sprintf "History of %s" (params_to_string _p) in
  Vcs.get_changes prefix ?path:_a >>= fun changes ->
  let links = link_to_atom _p in
  let a =
    object
      method title = title;
      method changes = changes;
      method links = links;
      method _p = _p;
      method hostname = hostname;
    end in
  render_with_layout History.f a;

value render_full_change hostname hash =
  let title = sprintf "Full patch %s" hash in
  Vcs.get_full_diff prefix hash >>= fun diff ->
  Vcs.wdiff diff >>= fun wdiff ->
  let wdiff = Vcs.xhtml_of_full_wdiff wdiff in
  let a =
    object
      method wdiff = wdiff;
      method title = title;
      method links = "";
      method hostname = hostname;
    end in
  render_with_layout View_full_change.f a;

value render_change hostname _p hash =
  let _a = get_path prefix _p in
  let title = sprintf "Patch %s" hash in
  Vcs.get_diff prefix _a hash >>= fun diff ->
  Vcs.wdiff diff >>= fun wdiff ->
  let wdiff = Vcs.xhtml_of_wdiff wdiff in
  let a =
    object
      method links = link_to_atom _p;
      method wdiff = wdiff;
      method title = title;
      method hostname = hostname;
      method hash = hash;
      method _p = _p;
    end in
  render_with_layout View_change.f a;

(* Makes a page with a list of directory content *)
value render_index hostname prefix _p =
  (* Returns true if file is invisible *)
  let invisible filename =
    if filename = "style.css" or filename = "_darcs"
    then True
    else if filename.[0] = '.'
      then True
      else False in
  let files = Sys.readdir (get_path prefix _p) in
  Lwt_list.fold_left_s
    (fun acc x ->
      let path = get_path prefix (_p @ [x]) in
      if invisible x then
        return acc
      else
        match Io.kind_of_file path (_p @ [x]) with
        [ Io.Page ->
            Markup.get_header_of_page path >>= fun
            [ Some s -> return s
            | None   -> return x ]
        | Io.Dir -> return (x ^ "/")
        | _ -> return x ] >>= fun title ->
        return ("<li>" ^ link_to (params_to_string (_p @ [x])) title ^ "</li>" ^ acc)
    ) "" (Array.to_list files) >>= fun lis ->
  let dirname =
    params_to_string _p in
  let title = "Index of " ^ dirname in
  let a =
    object
      method links = link_to_atom _p;
      method lis = lis;
      method title = title;
      method hostname = hostname;
      method _p = _p;
    end in
  render_with_layout Index.f a;

(* error 404 page *)
value render_404 =
  let a =
    object
      method title = "error 404: not found";
      method content =
        "<h1>error 404: not found</h1>\n<p>no such page</p>";
      method links = ""; (* FIXME *)
    end in
  render (Error.f a);

(* error 410 page *)
value render_410 path =
  let a =
    object
      method title = "error 410: gone";
      method content =
        sprintf "<h1>error 410: gone</h1>\n<p>This page has been removed.</p><p>But you still can browse its %s.</p>" (link_to_history path);
    end in
  render (Error.f a);

value render_500 =
  let a =
    object
      method title = "error 500: Internal Server Error";
      method content =
        sprintf "<h1>error 500: Internal Server Error</h1>\n<p>Sorry, something happend with our webserver.</p>";
    end in
  render (Error.f a);

(* vvv amall kludges vvv *)
open Amall_http;
module IO = IO_Lwt;
module I = Iteratees.Make(IO);
(* ^^^ amall kludges ^^^ *)

value send_404 =
  { rs_status_code = 404
  ; rs_reason_phrase = "Not found"
  ; rs_headers = { rs_all = [] }
  ; rs_body = Body_string render_404
  };

value send_410 path =
  { rs_status_code = 410
  ; rs_reason_phrase = "Gone"
  ; rs_headers = { rs_all = [] }
  ; rs_body = Body_string (render_410 path)
  };

value send_500 =
  { rs_status_code = 500
  ; rs_reason_phrase = "Internal Server Error"
  ; rs_headers = { rs_all = [] }
  ; rs_body = Body_string render_500
  };

value send_file ?content_type path_to_file =
  let rs_all = [] in
  let rs_all =
    match content_type with
    [ None -> rs_all
    | Some s -> [("Content-Type", s)::rs_all] ] in
  let size = Int64.of_int & (Unix.stat path_to_file).Unix.st_size in
  { rs_status_code = 200
  ; rs_reason_phrase = "OK"
  ; rs_headers = { rs_all = rs_all }
  ; rs_body = File_contents path_to_file size
  };

value redirect_to path =
  let location_header = ("Location", path) in
  { rs_status_code = 301
  ; rs_reason_phrase = "Moved Permanently"
  ; rs_headers = { rs_all = [location_header] }
  (*; rs_body = No_body*)
  ; rs_body = Body_string "moved" (* FIXME *)
  };

value send_ok_with ?content_type body =
  let rs_all = [] in
  let rs_all =
    match content_type with
    [ None -> rs_all
    | Some s -> [("Content-Type", s)::rs_all] ] in
  { rs_status_code = 200
  ; rs_reason_phrase = "OK"
  ; rs_headers = { rs_all = rs_all }
  ; rs_body = Body_string body
  };

value main_handler hostname _p =
  let _a = get_path prefix _p in
  match Io.kind_of_file _a _p with
  [ Io.Page when (try List.last _p = "index" with _ -> False) ->
      (* do not handle /path/index *)
      return & redirect_to & params_to_string &
        List.rev & List.drop 1 & List.rev _p
  | Io.Page ->
      render_article hostname _a _p >>= fun body ->
      Lwt.return & send_ok_with body
  | Io.Other | Io.Html | Io.VcsFile ->
      return & send_file _a
  | Io.Dir ->
      let index_a = _a ^/ "index" in
      let index_p = _p @ ["index"] in
      match Io.kind_of_file index_a index_p with
      [ Io.Page ->
          render_article hostname index_a index_p >>= fun body ->
          return & send_ok_with body
      | _ ->
          render_index hostname prefix _p >>= fun body ->
          return & send_ok_with body ]
  | Io.NotExists ->
      let path = gikia_public_dir ^/ params_to_string _p in
      if (Sys.file_exists path)
      then return & send_file path
      else
        (*match _p with
        [ ["sitemap.xml"] ->
            I.lift &
              application/xml
              Sitemap.make main_service prefix >>= fun body ->
              send_ok_with ~content_type:"application/xml" body
        | _ ->*)
            Vcs.where_file_leaves prefix _a _p >>= fun
            [ None -> return send_404
            | Some Vcs.Removed -> return (send_410 _p)
            | Some (Vcs.MovedTo dst) ->
                return & redirect_to (params_to_string dst) ] (* ] *)
  | Io.Incorrect -> return send_404 ];

value send_source _p =
  let _a = get_path prefix _p in
  match Io.kind_of_file _a _p with
  [ Io.Page ->
      return & send_file ~content_type:"text/plain" _a
  | Io.NotExists ->
      Vcs.where_file_leaves prefix _a _p >>= fun
      [ None -> return send_404
      | Some Vcs.Removed -> return (send_410 _p)
      | Some (Vcs.MovedTo dst) ->
          return & redirect_to (url_to_src dst) ]
  | _ -> return send_404 ];

value send_full_atom hostname =
  let make_iri hash = absolutify hostname & url_to_full_change hash in
  let link = absolutify hostname & url_to_history [] in
  let title = mainfeed_title hostname in
  Atom.of_repo ~title ~link make_iri prefix >>= fun atom ->
  return & send_ok_with ~content_type:"application/atom+xml" atom;

value send_atom hostname _p =
  let make_iri hash = absolutify hostname & url_to_full_change hash in
  let link = absolutify hostname & url_to_history _p in
  let title = sprintf "Recent changes to %s" (params_to_string _p) in
  let _a = get_path prefix _p in
  Atom.of_page ~title ~link make_iri prefix _a >>= fun atom ->
  return & send_ok_with ~content_type:"application/atom+xml" atom;

(* vvv amall kludges vvv *)
open Am_All;
open Amall_types;

module S = Amall_http_service.Service(IO)(I);

value (my_listener, http_root, _ws_root) = S.listener_create (`Inet_any port.val);

value my_endpoint =
  ( http_root
  , `Fallback []
  )
;

value my_func segpath rq =
  let (hostname, segpath) =
    match segpath with
    [ [_; hostname :: t] -> (hostname, t)
    | _ -> assert False ] in
  let hostname =
    match default_host.val with
    [ "" -> hostname
    | s -> s ] in
  let segpath =
    segpath >>
    List.filter (fun [ "" -> False | _ -> True ]) >>
    List.map Cd_Strings.Strings.Onebyte.urldecode >>
    (* stupid dtfilter *)
    List.filter (fun x -> not (ExtLib.String.exists x "..")) in
  let params =
    match rq.rq_uri.Uri_type.query with
    [ None -> []
    | Some s -> Uri.parse_params s ] in
  I.lift &
  catch (fun () ->
    match (segpath, params) with
    [ ([],      [("show", "log")]) ->
        render_full_history hostname >>= fun body ->
        Lwt.return & send_ok_with body
    | (segpath, [("show", "log")]) ->
        render_history hostname segpath >>= fun body ->
        Lwt.return & send_ok_with body
    | ([],      [("show", "atom")]) -> send_full_atom hostname
    | (segpath, [("show", "atom")]) -> send_atom hostname segpath
    | (segpath, [("show", "source")]) -> send_source segpath
    | ([],      [("hash", hash)]) ->
        render_full_change hostname hash >>= fun body ->
        Lwt.return & send_ok_with body
    | (segpath, [("hash", hash) :: _]) ->
        render_change hostname segpath hash >>= fun body ->
        Lwt.return & send_ok_with body
    | (segpath, []) -> main_handler hostname segpath
    | _ -> Lwt.return send_404 ] )
  (fun e -> Lwt.return send_500)
;

value () = S.mount_http my_endpoint my_func;

value () = S.listener_run my_listener;
(* ^^^ amall kludges ^^^ *)
