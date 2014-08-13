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

(** misc fucntions *)

open ExtLib

(* Useful combinators *)

let (>>) f g = g f
let (^/) = Filename.concat
let (!!) = Lazy.force
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

module C = CalendarLib.Calendar.Precise

let rfc3339_of_calendar c =
  let module C = CalendarLib.Calendar.Precise in
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
  CalendarLib.Printer.Precise_Calendar.sprint "%a, %d %b %Y %T %z" c

(*(* haven't read http://tools.ietf.org/html/rfc2822#section-3.3
 * who cares *)
let rfc2282_of_calendar =
  CalendarLib.Printer.Precise_Calendar.sprint
    "%a, %d %b %Y %H:%M:%S %z"*)

(* "Sun, 08 Dec 2013 22:27:45 +0400" *)
let calendar_of_rfc2282 s =
  let module C = CalendarLib.Calendar.Precise in
  String.nsplit s " "
  >> List.filter (function "" -> false | _ -> true) >>
  function
  | [day_of_week; day; month; year; time; timezone] ->
      let day = int_of_string day in
      let month =
        String.slice ~first:0 ~last:3 month
        >> (function
          | "Jan" -> C.Jan
          | "Feb" -> C.Feb
          | "Mar" -> C.Mar
          | "Apr" -> C.Apr
          | "May" -> C.May
          | "Jun" -> C.Jun
          | "Jul" -> C.Jul
          | "Aug" -> C.Aug
          | "Sep" -> C.Sep
          | "Oct" -> C.Oct
          | "Nov" -> C.Nov
          | "Dec" -> C.Dec
          | _ -> failwith "calendar_of_rfc2282") >>
        C.Date.int_of_month in
      let year = int_of_string year in
      let hours, minutes, seconds =
        match String.nsplit time ":" with
        | [hours; minutes; seconds] ->
            int_of_string hours,
            int_of_string minutes,
            int_of_string seconds
        | _ -> failwith "calendar_of_rfc2282" in
      let timezone =
        match timezone with
        | "GMT" -> CalendarLib.Time_Zone.UTC
        | _ ->
            let mult =
              match timezone.[0] with
              | '+' -> 1
              | '-' -> -1
              | _ -> failwith "calendar_of_rfc2282" in
            let hours =
              String.slice ~first:1 ~last:3 timezone >>
              int_of_string in
            CalendarLib.Time_Zone.UTC_Plus (hours * mult) in
      let c =
        CalendarLib.Time_Zone.on
          (fun () -> C.make year month day hours minutes seconds)
          timezone
          () in
      (c, timezone)
  | _ -> failwith "calendar_of_rfc2282"

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


let fix_broken_ascii s =
  String.map (fun c -> if int_of_char c < 32 then ' ' else c) s


