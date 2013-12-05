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

(* Useful combinators *)

let (>>) f g = g f
let (^/) = Filename.concat
let hdtl l = (List.hd l, List.tl l)

(* Directory traversal filter *)
let dtfilter l =
  List.fold_right (fun x acc ->
    match x with
    | "" | "." -> acc
    | _ when String.exists x ".." -> acc
    | _ -> x :: acc) l []

(* String functions *)

let find_from str pos sub =
  let sublen = String.length sub in
  if sublen = 0 then
    0
  else
    let found = ref pos in
    let len = String.length str in
    try
      for i = pos to len - sublen do
        let j = ref 0 in
        while String.unsafe_get str (i + !j) = String.unsafe_get sub !j do
          incr j;
          if !j = sublen then begin found := i; raise Exit; end;
        done;
      done;
      raise Invalid_string
    with
      Exit -> !found

let replace ?beg ~str ~sub ~by =
  let strlen = String.length str
  and sublen = String.length sub in
  let buf = Buffer.create (String.length str) in
  let rec loop n =
    try
      let pos = find_from str n sub in
      Buffer.add_substring buf str n (pos - n);
      Buffer.add_string buf by;
      let newn = (pos + sublen) in
      if newn > strlen
      then ()
      else loop newn
    with Invalid_string ->
      Buffer.add_substring buf str n (strlen - n) in
  match beg with
  | None ->
      loop 0;
      Buffer.contents buf
  | Some beg when strlen < beg ->
      str
  | Some beg ->
      Buffer.add_substring buf str 0 beg;
      loop beg;
      Buffer.contents buf

(* ' -> '\'' *)
let quote s =
  replace s "'" "'\\''"

(* List functions *)

let rec exude p = function
  | [] -> raise Not_found
  | x :: l -> (match p x with
      | Some y -> y
      | None -> exude p l)

(* Calendar *)

let rfc3339_of_calendar c =
  let module C = CalendarLib.Calendar in
  let module D = CalendarLib.Date in
  let month x = match C.month x with
  |D.Jan -> 1 |D.Feb -> 2 |D.Mar -> 3
  |D.Apr -> 4 |D.May -> 5 |D.Jun -> 6
  |D.Jul -> 7 |D.Aug -> 8 |D.Sep -> 9
  |D.Oct -> 10|D.Nov -> 11|D.Dec -> 12 in
  Printf.sprintf "%.4d-%.2d-%.2dT%.2d:%.2d:%.2dZ"
    (C.year c) (month c) (C.day_of_month c) (C.hour c)
    (C.minute c) (C.second c)

let rfc822_of_calendar c =
  CalendarLib.Printer.Calendar.sprint "%a, %d %b %Y %T %z" c


(*(* haven't read http://tools.ietf.org/html/rfc2822#section-3.3
 * who cares *)
let rfc2282_of_calendar =
  CalendarLib.Printer.Calendar.sprint
    "%a, %d %b %Y %H:%M:%S %z"*)

(* html escaping *)
let esc s =
  let strlen = String.length s in
  let buf = Buffer.create strlen in
  let f = function
    | '&' -> Buffer.add_string buf "&amp;"
    | '<' -> Buffer.add_string buf "&lt;"
    | '>' -> Buffer.add_string buf "&gt;"
    | '"' -> Buffer.add_string buf "&quot;"
    |  c  -> Buffer.add_char buf c in
  String.iter f s;
  Buffer.contents buf

let esc_to_buf buf s =
  let f = function
    | '&' -> Buffer.add_string buf "&amp;"
    | '<' -> Buffer.add_string buf "&lt;"
    | '>' -> Buffer.add_string buf "&gt;"
    | '"' -> Buffer.add_string buf "&quot;"
    |  c  -> Buffer.add_char buf c in
  String.iter f s

(** применить значение к функции:
    print_string & string_of_int & 123

    NB: оператор "&" является ключевым словом в jocaml

    Если попробовать объявить "let ( $ ) f x = f x",
    то полученный оператор будет левоассоциативным,
    что нежелательно в данном случае.
*)
let ( & ) f x = f x

let urlencode = Cd_Strings.Strings.Onebyte.urlencode

(* Translate parameters to a string
 * ["one", "two"] -> "one/two" *)
let params_to_string params =
  List.fold_left Filename.concat "/" params

let absolutify hostname path =
  "http://" ^ hostname ^ path

let link_to url inner =
  Printf.sprintf "<a href=\"%s\">%s</a>" (esc url) (esc inner)

(* Returns an absolute path to the page in filesystem *)
let get_path prefix params =
  prefix ^/ (params_to_string params)

let url_to_change _p hash =
  (params_to_string _p) ^ "?hash=" ^ urlencode hash

let url_to_full_change hash =
  "/?hash=" ^ urlencode hash


