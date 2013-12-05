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

open Printf;
open Lwt;
open Utils;

value run cmd =
  Lwt_unix.with_timeout 5.0 (fun () ->
    Lwt_process.shell cmd >>
    Lwt_process.pread_line);

(* looking for toolkits in environment *)
value () =
  (* for running 'utilname --version' *)
  (* writes m in log if f returns false *)
  let check f m =
    catch
      (fun () -> f () >>= fun
        [ True  -> return ()
        | False -> return (print_endline m)]) (* FIXME *)
      (fun _ -> return (print_endline m)) in (* FIXME *)

  ((*check (fun () ->
      run "polebrush -help" >>= fun line ->
      Scanf.sscanf line "polebrush markup language formatter" (return True))
    "you need polebrush" >>= fun () ->*)
  check (fun () ->
      run "source-highlight --version" >>= fun line ->
      Scanf.sscanf line "GNU Source-highlight" (return True))
    "you need source-highlight" >>= fun () ->
  check (fun () ->
      run "wdiff -v" >>= fun line ->
      let f x y _ = return (x > 0 || y >= 6) in
      Scanf.sscanf line "wdiff (GNU wdiff) %d.%d.%d" f)
    "you need wdiff >= 0.6.0" >>= fun () ->
  check (fun () ->
      run "darcs --version" >>= fun line ->
      let f x y _ = return (x >= 2 && y >= 4) in
      Scanf.sscanf line "%d.%d.%d" f)
    "you need darcs >= 2.4.0" >>= fun () ->
  check (fun () ->
      run "git --version" >>= fun line ->
      let f x y _ = return (x >= 1) in
      Scanf.sscanf line "git version %d.%d.%d" f)
    "you need git >= 1.0.0")

  >> Lwt_main.run;

open Arg;

value port = ref 8080;
value default_host = ref "";
value markup = ref "";
value escape_html = ref True;
value prefix = ref "";

value () =
  let help =
    "gikia - a simple wiki engine\n" in
  let l = [
    ("-p", Set_int port, "INT\tport to listening; default is 8080");
    ("-default-host", Set_string default_host, "STR\tdefault hostname; useful for production environment");
    ("-markup", Set_string markup, "STR\tset markup language: polebrush (default), textile or pure html");
    ("-escape-html", Bool (fun b -> escape_html.val := b), "BOOL\tescape html among markup; default is true; does nothing with -markup html");
  ] in
  Arg.parse l (fun x -> prefix.val := x) help;

value prefix =
  match prefix.val with
  [ "" ->
      begin eprintf "you need to specify a path to wiki\n"; exit 1 end
  | s -> s ];

value markup =
  let default = Markup.Polebrush in
  try
    match markup.val with
    [ "textile" | "Textile" | "TEXTILE" -> Markup.Textile
    | "polebrush" | "Polebrush" | "POLEBRUSH" -> Markup.Polebrush
    | "html" | "Html" | "HTML" | "xhtml" | "Xhtml" | "XHTML" -> Markup.Html
    | _ -> default ]
  with _ -> default;

module Markup = Markup.Make (struct
  value markup = markup;
  value escape_html = escape_html.val;
end);

value gikia_public_dir = Filename.dirname Sys.argv.(0) ^/ "public";

