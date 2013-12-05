(*
 * Copyright (c) 2005 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2010 Alexander Markov <aspheronets@gmail.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * $Id: atom.ml,v 1.2 2005/09/20 19:09:06 avsm Exp $
 *)

open Lwt
open Utils
open CalendarLib

let entry_of_change make_iri prefix path c =
  Vcs.get_diff prefix path c.Vcs.hash >>= fun diff ->
  Vcs.wdiff diff >>= fun wdiff ->
  let xhtml =
    "<div><pre class=\"diff\">" ^
    Vcs.xhtml_of_full_wdiff wdiff ^
    "</pre></div>" in
  let updated = match c.Vcs.date with
    | Vcs.Rfc s -> s
    | Vcs.Calendar d -> rfc3339_of_calendar d in
  let iri = make_iri c.Vcs.hash in
  return (
    "<entry>" ^
      "<summary type=\"html\">" ^ esc xhtml ^ "</summary>" ^
      "<title>" ^ esc c.Vcs.title ^ "</title>" ^
      "<updated>" ^ esc  updated ^ "</updated>" ^
      "<id>" ^ esc iri ^ "</id>" ^
      "<link href=\"" ^ esc iri ^ "\" />" ^
    "</entry>"
  )

let of_changes ~title ~link make_iri prefix path changes =
  Lwt_list.map_s (entry_of_change make_iri prefix path) changes >>= fun entries ->
  let up =
    let f =
      try (Unix.stat path).Unix.st_mtime
      with _ -> 0. in
    rfc3339_of_calendar (Calendar.from_unixfloat f) in
  return (
    "<feed xmlns=\"http://www.w3.org/2005/Atom\">" ^
      "<title>" ^ esc title ^ "</title>" ^
      "<updated>" ^ esc up ^ "</updated>" ^
      "<id>" ^ esc link ^ "</id>" ^
      "<link href=\"" ^ esc link ^ "\" />" ^
      String.concat "" entries ^
    "</feed>"
  )

let of_page ~title ~link make_iri prefix path =
  Vcs.get_changes prefix ~count:20 ~path >>= fun changes ->
  of_changes ~title ~link make_iri prefix path changes

let of_repo ~title ~link make_iri prefix =
  Vcs.get_changes prefix ~count:20 >>= fun changes ->
  of_changes ~title ~link make_iri prefix prefix changes

