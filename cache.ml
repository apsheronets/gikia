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

open Lwt
open Utils
open Unix

type mtime =
  | Mtime of float
  | AlwaysFresh (* for dynamic stuff *)

let http_header_of_mtime mtime =
  let c = CalendarLib.Calendar.Precise.from_unixfloat mtime in
  let rfc822 = Utils.rfc822_of_calendar c in
  ("Last-Modified", rfc822)

type 'a chunk = {
  mtime: mtime;
  body: 'a Lazy.t;
}

let just_modified_header () =
  let c = CalendarLib.Calendar.Precise.now () in
  let rfc822 = Utils.rfc822_of_calendar c in
  ("Last-Modified", rfc822)

let http_header_of_chunk chunk = (* should I remove this function? *)
  match chunk.mtime with
  | AlwaysFresh -> just_modified_header ()
  | Mtime x -> http_header_of_mtime x

let merge_mtimes x y =
  match x, y with
  | AlwaysFresh, _ -> AlwaysFresh
  | _, AlwaysFresh -> AlwaysFresh
  | Mtime x, Mtime y -> Mtime (if x > y then x else y)

let merge_chunks x y f = {
  mtime = merge_mtimes x.mtime y.mtime;
  body = lazy (f !!(x.body) !!(y.body))
}

