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

value () =
  Lwt_preemptive.init 0 32 (fun s -> Printf.printf "Lwt threads log: %s\n%!" s);

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
        | False -> Lwt_io.eprintf "%s\n" m]) (* FIXME *)
      (fun _ -> Lwt_io.eprintf "%s\n" m) in (* FIXME *)

  ((*check (fun () ->
      run "polebrush -help" >>= fun line ->
      Scanf.sscanf line "polebrush markup language formatter" (return True))
    "you need polebrush" >>= fun () ->*)
  check (fun () ->
      run "source-highlight --version" >>= fun line ->
      Scanf.sscanf line "GNU Source-highlight" (return True))
    "source-highlight is missing! You need to install source-highlight (https://www.gnu.org/software/src-highlite/source-highlight.html) if you want code highlighting in polebrush" >>= fun () ->
  check (fun () ->
      run "wdiff -v" >>= fun line ->
      let f x y _ = return (x > 0 || y >= 6) in
      Scanf.sscanf line "wdiff (GNU wdiff) %d.%d.%d" f)
    "wdiff is missing! You need to install wdiff >= 0.6.0 to render beautifull diffs" >>= fun () ->
  check (fun () ->
      run "darcs --version" >>= fun line ->
      let f x y _ = return (x >= 2 && y >= 4) in
      Scanf.sscanf line "%d.%d.%d" f)
    "darcs is missing! You need to install darcs >= 2.4.0 if you want to use darcs repos" >>= fun () ->
  check (fun () ->
      run "git --version" >>= fun line ->
      let f x y _ = return (x >= 1) in
      Scanf.sscanf line "git version %d.%d.%d" f)
    "git is missing! You need to install git >= 1.0.0 if you want to use git")

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
    ("-p", Set_int port, "INT\t\tport to listening; default is 8080");
    ("-default-host", Set_string default_host, "STR\tdefault hostname; useful for production environment");
    ("-markup", Set_string markup, "STR\t\tset markup language: polebrush (default), textile or pure html");
    ("-escape-html", Bool (fun b -> escape_html.val := b), "BOOL\tescape html among markup; default is true; does nothing with -markup html");
  ] in
  Arg.parse l (fun x -> prefix.val := x) help;

value prefix =
  match prefix.val with
  [ "" ->
      begin eprintf "You need to specify a path to wiki\n"; exit 1 end
  | s -> s ];

value gikia_public_dir = Filename.dirname Sys.argv.(0) ^/ "public";

