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

open Printf
open Utils

let url_to_history p =
  params_to_string p ^ "?show=log"
let link_to_history p =
  link_to (url_to_history p) "history"
let url_to_src p =
  params_to_string p ^ "?show=source"
let link_to_src p =
  link_to (url_to_src p) "source"
let url_to_atom p =
  params_to_string p ^ "?show=atom"
let link_to_atom p =
  sprintf "<link rel=\"alternate\" type=\"application/atom+xml\" title=\"Recent changes to this page\" href=\"%s\" />" (url_to_atom p)

