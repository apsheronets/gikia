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

open ExtLib
open Utils
open Routes
open Lwt

let render ?(b = 9999) f =
  let b = Buffer.create b in
  let () = f b in
  Buffer.contents b

let render_with_layout ?(b = 9999) f a =
  let b = Buffer.create b in
  Layout.f a (f a) b >|= fun () ->
  Buffer.contents b

let render_breadcrumbs = function
  | [] | ["index"] -> ""
  | params ->
      let l = List.length params in
      let rec loop acc i =
        if i <= l then
          let p = List.take i params in
          let h = List.last p in
          let link = link_to (params_to_string p) (esc h) in
          loop (acc ^ " » " ^ link) (i+1)
        else acc in
      loop (link_to "/" "main") 1

let render_editlinks p =
  link_to_history p ^ " | " ^ link_to_src p

