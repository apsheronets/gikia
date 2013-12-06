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
  let p = Lwt_process.open_process_full ~timeout cmd in
  Lwt_io.read p#stdout >>= fun out ->
  Lwt_io.read p#stderr >>= fun err ->
  p#close >>= fun _ ->
  if String.length err = 0
  then return out
  else fail (Error (sprintf "%s: %s" (string_of_cmd cmd) err))

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

(* Getting type of a file with magic library *)
let magic_cookie =
  let flags = [ Magic.Mime; Magic.Symlink ] in
  Magic.make ~flags []

let magic_file path =
  let f path =
    Magic.file magic_cookie path in
  Lwt_preemptive.detach f path

type kind_of_file = Page | Other | Html | Dir | NotExists | Incorrect | VcsFile
(* Returns a type of the file *)
let kind_of_file path params =
  let name = try List.last params with List.Empty_list -> "" in
  match name with
  | ".." | "." -> return Incorrect
  | _ -> (
      match params with
      | "_darcs"::_ -> return VcsFile
      | ".git"  ::_ -> return VcsFile
      | ["robots.txt"] -> return Other
      | ["style.txt"]  -> return Other
      | _ ->
          catch (fun () ->
            print_endline path;
            Lwt_unix.lstat path >>= fun stats ->
            let st_kind = stats.Unix.st_kind in
            match st_kind with
            | Unix.S_DIR -> return Dir
            | Unix.S_REG -> (
                if String.ends_with name ".html"
                then return Html
                else
                  catch (fun () ->
                    magic_file path >>= fun filetype ->
                    print_endline filetype;
                    if   String.starts_with filetype "text"
                      || String.starts_with filetype "application/octet-stream"
                    then return Page
                    else return Other)
                  (function Magic.Failure _ -> return Other | e -> fail e))
            | _ -> return Incorrect)
          (function Unix.Unix_error _ -> return NotExists | e -> fail e))

