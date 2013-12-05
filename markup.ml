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
open Printf
open ExtLib

type t = Textile | Polebrush | Html

module type Options = sig
  val markup : t
  val escape_html : bool
end

module Make(Opt: Options) = struct

  module type MARKUP =
  sig
    val get_header_of_page : string -> string option Lwt.t
    val get_page : bool -> string -> (string option * string) Lwt.t
  end

  module Textile : MARKUP =
  struct

    let get_header_from_textile textile =
      match Stream.peek textile with
      | Some (Textile.Header (_, (_, l::_))) ->
          Some (Textile.string_of_line l)
      | _ -> None

    (* preemptive function *)
    let get_header_of_page _a =
      let f _a =
        let chan = open_in _a in
        let lines = Stream.from (fun _ ->
          try Some (input_line chan)
          with End_of_file -> None) in
        let textile = Textile_parser.of_stream lines in
        let result = get_header_from_textile textile in
        close_in chan;
        result in
      Lwt_preemptive.detach f _a

    let string_of_textile escape_cdata tt =
      let f = function
        | Textile.Blockcode ((attrs, _, _), strings) ->
            let linenums = List.length strings > 1 in
            let lang = try Some (exude (function
              | Textile.Language s -> Some s
              | _ -> None) attrs) with Not_found -> None in
            let s = String.concat "\n" strings in
            Highlight.to_string ?lang linenums s
        | b -> Textile_html.of_block ~escape_cdata b in
      let next _ =
        try
          Some (f (Stream.next tt))
        with Stream.Failure -> None in
      let stream = Stream.from next in
      let buf = Buffer.create 1024 in
      Stream.iter (Buffer.add_string buf) stream;
      Buffer.contents buf

    (* preemptive function *)
    let get_page escape_html _a =
      let f () =
        let chan = open_in _a in
        let lines = Stream.from (fun _ ->
          try Some (input_line chan)
          with End_of_file -> close_in chan; None) in
        let tt = Textile_parser.of_stream lines in
        let title = get_header_from_textile tt in
        string_of_textile escape_html tt >> fun content ->
        title, content in
      Lwt_preemptive.detach f ()

  end

  module Polebrush : MARKUP =
  struct

    let get_header_from_polebrush polebrush =
      match Enum.peek polebrush with
      | Some (Polebrush.Header (_, (_, lines))) ->
          Some (String.concat " " (List.map Polebrush.string_of_line lines))
      | _ -> None

    (* preemptive function *)
    let get_header_of_page _a =
      let f _a =
        let chan = open_in _a in
        let lines = Stream.from (fun _ ->
          try Some (input_line chan)
          with End_of_file -> None) in
        let pb = Polebrush_parser.enum lines in
        let result = get_header_from_polebrush pb in
        close_in chan;
        result in
      Lwt_preemptive.detach f _a

    let string_of_polebrush escape_cdata pb =
      (*let enum =
        Enum.from (fun () ->
          try Stream.next stream
          with Stream.Failure -> raise Enum.No_more_elements) in*)
      let toc, enum = Polebrush_html.toc_of_enum pb in
      let f = Polebrush_html.of_block ~code_highlight_cmd:"source-highlight -t 2 -o STDOUT -s" ~toc ~escape_cdata in
      let next _ =
        match Enum.get enum with
        | Some b -> Some (f b)
        | None -> None in
      let stream = Stream.from next in
      let buf = Buffer.create 1024 in
      Stream.iter (Buffer.add_string buf) stream;
      Buffer.contents buf

    (* preemptive function *)
    let get_page escape_html _a =
      let f () =
        let chan = open_in _a in
        let lines = Stream.from (fun _ ->
          try Some (input_line chan)
          with End_of_file -> close_in chan; None) in
        let pb = Polebrush_parser.enum lines in
        let title = get_header_from_polebrush pb in
        string_of_polebrush escape_html pb >> fun content ->
        title, content in
      Lwt_preemptive.detach f ()

  end

  module Html : MARKUP =
  struct
    let get_header_of_page _a = return None
    let get_page _ _a =
      get_header_of_page _a >>= fun title ->
      Io.read _a >>= fun content ->
      return (title, content)
  end

  let get_header_of_page =
    match Opt.markup with
    | Textile -> Textile.get_header_of_page
    | Polebrush -> Polebrush.get_header_of_page
    | Html -> Html.get_header_of_page

  let get_page =
    match Opt.markup with
    | Textile -> Textile.get_page Opt.escape_html
    | Polebrush -> Polebrush.get_page Opt.escape_html
    | Html -> Html.get_page Opt.escape_html

end


