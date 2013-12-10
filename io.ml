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
open Init
open Utils
open Printf
open ExtLib

let timeout = 10.0

exception Error of string

let string_of_cmd (name, args) =
  let b = Buffer.create 42 in
  bprintf b "launching %S:" name;
  Array.iter (bprintf b " %S") args;
  Buffer.contents b

let exec ?(timeout=timeout) cmd =
  (*Lwt_io.printl (string_of_cmd cmd) >>= fun () ->*)
  let p = Lwt_process.open_process_full ~timeout cmd in
  Lwt_io.read p#stdout >>= fun out ->
  Lwt_io.read p#stderr >>= fun err ->
  p#close >>= fun _ ->
  if String.length err = 0
  then return out
  else fail (Error (sprintf "%s: %s" (string_of_cmd cmd) err))

let pool = Lwt_pool.create 50 (fun () -> return ())

let exec ?timeout cmd =
  Lwt_pool.use pool (fun () -> exec ?timeout cmd)

let open_process_full ?timeout cmd =
  Lwt_pool.use pool (fun () -> return & Lwt_process.open_process_full ?timeout cmd)

let pread_lines ?timeout cmd =
  Lwt_pool.use pool (fun () -> return & Lwt_process.pread_lines ?timeout cmd)

let read path =
  Lwt_io.with_file
    Lwt_io.input
    path
    (fun inp ->
      Lwt_io.read inp)

let try_read_file p =
  Lwt.catch
    (fun () -> read p)
    (fun _ -> Lwt.return "")

let file_exists path =
  catch
    (fun () ->
      Lwt_unix.lstat path >>= fun _ -> return true)
    (function _ -> return false)

let magic_file path =
  exec ("file", [|"file"; "--mime"; "-L"; "--brief"; path|])

type kind_of_file = Page | Other | Html | Dir | NotExists | Incorrect | VcsFile

(* Returns a type of the file *)
let kind_of_file segpath st_kind absolute_path =
  let name = try List.last segpath with List.Empty_list -> "" in
  match name with
  | ".." | "." -> return Incorrect
  | _ -> (
      match segpath with
      | "_darcs"::_ -> return VcsFile
      | ".git"  ::_ -> return VcsFile
      | ["robots.txt"] -> return Other
      | ["style.txt"]  -> return Other
      | _ ->
          catch (fun () ->
            Lazy.force st_kind >>= fun st_kind ->
            match st_kind with
            | Unix.S_DIR -> return Dir
            | Unix.S_REG -> (
                if String.ends_with name ".html"
                then return Html
                else
                  catch (fun () ->
                    magic_file (Lazy.force absolute_path) >>= fun filetype ->
                    if   String.starts_with filetype "text"
                      || String.starts_with filetype "application/octet-stream"
                    then return Page
                    else return Other)
                  (function _ (* magic failure :) *) -> return Other | e -> fail e))
            | _ -> return Incorrect)
          (function Unix.Unix_error _ -> return NotExists | e -> fail e))

(* Lazy, lazy file *)
class file ?(prefix=prefix) segpath =
  let path = lazy (params_to_string segpath) in
  let absolute_path = lazy (prefix ^/ !!path) in
  let exists =
    lazy (file_exists (!!absolute_path)) in
  let lstat = lazy (
    Lwt_unix.lstat (!!absolute_path)) in
  let kind_of_file =
    lazy (
      let st_kind = lazy (
        !!lstat >>= fun lstat ->
        return lstat.Unix.st_kind) in
      kind_of_file
        segpath st_kind absolute_path) in
  let size =
    lazy (
      !!lstat >>= fun lstat ->
      return & Int64.of_int & lstat.Unix.st_size) in

  object (self)
    method segpath = segpath
    method path = !!path
    method absolute_path = !!absolute_path
    method exists = !!exists
    method lstat = !!lstat
    method st_kind = self#lstat >|= fun s -> s.Unix.st_kind
    method mtime = self#lstat >|= fun s -> s.Unix.st_mtime
    method kind_of_file = !!kind_of_file
    method size = !!size
    method last_modified_header =
      self#mtime >|= fun mtime ->
      let c = CalendarLib.Calendar.from_unixfloat mtime in
      let rfc822 = Utils.rfc822_of_calendar c in
      ("Last-Modified", rfc822)
  end

