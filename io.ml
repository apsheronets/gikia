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

let counter = ref 0

let exec ?(timeout=timeout) cmd =
  printf "%s%!" (string_of_cmd cmd); (* TODO: add debug here *)
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

(* not cooperative functions *)

(* Getting type of a file with magic library *)
let magic_cookie =
  let flags = [ Magic.Mime; Magic.Symlink ] in
  Magic.make ~flags []

type kind_of_file = Page | Other | Html | Dir | NotExists | Incorrect | VcsFile
(* Returns a type of the file *)
let kind_of_file path params =
  if not (Sys.file_exists path) then NotExists else
  if Sys.is_directory path then Dir else
  let name = try List.last params with List.Empty_list -> "" in
  try
    let filetype = lazy (Magic.file magic_cookie path) in
    match name, params with
    |     "..",          _        -> Incorrect
    |     ".",           _        -> Incorrect
    |      _,         "_darcs"::_ -> VcsFile
    |      _,         ".git"::_   -> VcsFile
    |      _,      ["robots.txt"] -> Other
    | "style.css",       _        -> Other
    | _ when String.ends_with name ".html"  -> Html
    | _ when String.starts_with (Lazy.force filetype) "text" -> Page
    | _ when String.starts_with (Lazy.force filetype) "application/octet-stream" -> Page
    | _ -> Other
  with Magic.Failure _ -> Other

let kind_of_file path params =
  let f params = kind_of_file path params in
  Lwt_preemptive.detach f params
