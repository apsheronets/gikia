(* This file is part of polebrush.
 *
 * polebrush is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * polebrush is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with polebrush.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright 2011 Alexander Markov *)

open ExtString

type 'a parse_result = Parsed of 'a * (string * int) | Failed

let return x s = Parsed(x, s)
let fail s = Failed

let p_pred p (s, pos) =
  try
    let c = s.[pos] in
    if p c then Parsed (c, (s, succ pos)) else Failed
  with Invalid_argument _ -> Failed

let p_pred2 p (s, pos) =
  try
    let c = s.[pos] in
    match p c with
    | Some r -> Parsed (r, (s, succ pos))
    | None -> Failed
  with Invalid_argument _ -> Failed

let p_char c = p_pred ((=) c)

(** just returns current position; doesn't jump *)
let current_pos (s, pos) = Parsed (pos, (s, pos))

(** parsed if current position = p; doesn't jump *)
let p_pos p (s, pos) = if p = pos then return p (s, pos) else fail (s, pos)

(** sets position *)
let set_pos p (s, _) = Parsed ((), (s, p))

let dont_jump p (s, pos) =
  match p (s, pos) with
  | Parsed (r, _) -> Parsed (r, (s, pos))
  | Failed -> Failed

let p_begin (s, pos) =
  if pos = 0
  then Parsed ((), (s, pos))
  else Failed

let p_end (s, pos) =
  if String.length s = pos
  then Parsed ((), (s, pos))
  else Failed

let p_somechar (s, pos) =
  try
    let c = s.[pos] in
    Parsed (c, (s, succ pos))
  with Invalid_argument _ -> Failed

let p_manyf prs f v0 =
  let rec loop v st =
    match prs st with
    | Parsed (x, s') -> loop (f v x) s'
    | Failed -> Parsed (v, st) in
  loop v0

let p_manyf_arg prs f v0 =
  let rec loop v st =
    match prs v st with
    | Parsed (x, s') -> loop (f v x) s'
    | Failed -> Parsed (v, st) in
  loop v0

let p_many prs =
  let rec loop st =
    match prs st with
    | Parsed (_, s') -> loop s'
    | Failed -> Parsed ((), st) in
  loop

let p_upto_timesf times prs f v0 =
  let rec loop t v st =
    if t < times
    then
      match prs st with
      | Parsed (x, s') -> loop (succ t) (f v x) s'
      | Failed -> Parsed (v, st)
    else Parsed (v, st) in
  loop 0 v0

let p_opt defval p s =
  match p s with
  | Parsed _ as ok -> ok
  | Failed -> Parsed (defval, s)

let (|||) p1 p2 s =
  match p1 s with
  | Parsed _ as ok -> ok
  | Failed -> p2 s

let (>>=) p1 f s =
  match p1 s with
  | Parsed (x, s2) -> f x s2
  | Failed -> Failed

let (>>>) p1 p2 s =
  match p1 s with
  | Parsed (_, s2) -> p2 s2
  | Failed -> Failed

let p_plus prs = prs >>> p_many prs

let p_manyf prs f v0 =
  let rec loop v st =
    match prs st with
    | Parsed (x, s') -> loop (f v x) s'
    | Failed -> Parsed (v, st) in
  loop v0

let p_manyf_ends_with prs f v0 e =
  let rec loop v st =
    match prs st with
    | Parsed (x, s') -> loop (f v x) s'
    | Failed ->
        (match e st with
        | Parsed (_, st) -> Parsed (v, st)
        | Failed -> Failed) in
  loop v0

let p_plusf prs f v0 =
  prs >>= fun x -> return (f v0 x) >>= p_manyf prs f

let isdigit c = c>='0' && c<='9'
let p_digit = p_pred isdigit

let mkInt v x = v * 10 + int_of_char x - 48

let p_unsign_int = p_plusf p_digit mkInt 0

let p_int (s, pos) =
  try
    let c = s.[pos] in
    let t = (s, succ pos) in
    match c with
    | '-' ->
        (match p_manyf p_digit mkInt 0 t with
        | Parsed (x, s') -> Parsed (-x, s')
        | Failed -> Failed)
    | '0'..'9' -> p_manyf p_digit mkInt 0 (s, pos)
    | _ -> Failed
  with Invalid_argument _ -> Failed

let p_str str =
  String.fold_left (fun p c -> p >>> p_char c) (return '!') str;;

let p_ending (s, pos) =
  Parsed ((String.slice ~first:pos s), (s, String.length s))

let p_rest = p_ending

(** sequence of something *)
let rec p_seq prs =
  p_manyf prs (fun acc x -> x :: acc) [] >>= fun rl ->
  return (List.rev rl)

(* too slow, disabled *)
(*let rec p_seq1 prs = (* sequence of something *)
  prs >>= fun x ->
  p_opt [x] (p_seq1 prs >>= fun lst -> return (x::lst))*)

let rec p_seq1 prs =
  prs >>= fun v0 ->
  p_manyf prs (fun acc x -> x :: acc) [v0] >>= fun rl ->
  return (List.rev rl)

let rec p_list prs psep = (* list of something, separated by given separator parser *)
  prs >>= fun x ->
  p_opt [x] (psep >>> p_list prs psep >>= fun lst -> return (x::lst))

let rec p_listch prs sep = (* list of something, separated by given char *)
  prs >>= fun x ->
  p_opt [x] (p_char sep >>> p_listch prs sep >>= fun lst -> return (x::lst))

let p_intlist = p_listch p_int;;

let p_void prs s =
  match prs s with
  | Parsed(_, s') -> Parsed((), s')
  | Failed -> Failed

let mkFloat (fv,fr) c = fv +. float_of_int (int_of_char c - 48) *. fr , fr *. 0.1

let p_float =
  p_opt 1.0 (p_char '-' >>> return (-1.0)) >>= fun sign ->
  p_manyf p_digit mkInt 0 >>= fun n ->
  p_char '.' >>>
  p_manyf p_digit mkFloat (0.0, 0.1) >>= fun (fv, _) ->
  return (sign *. (float_of_int n +. fv))

let p_str_until until (s, pos) =
  let beg = pos in
  let rec loop (s, pos) =
    match until (s, pos) with
    | Parsed (_, (s, new_p)) ->
        Parsed (String.slice ~first:beg ~last:pos s, (s, new_p))
    | Failed ->
        (* FIXME *)
        if pos = String.length s
        then Failed
        else loop (s, succ pos) in
  loop (s, pos)

let p_str_until_p until (s, pos) =
  let beg = pos in
  let rec loop (s, pos) =
    match until (s, pos) with
    | Parsed (until_r, (s, new_p)) ->
        Parsed ((String.slice ~first:beg ~last:pos s, until_r), (s, new_p))
    | Failed ->
        (* FIXME *)
        if pos = String.length s
        then Failed
        else loop (s, succ pos) in
  loop (s, pos)

(* seems like a replacement for parsec's manyTill *)
let p_until p until (s, pos) =
  let beg = pos in
  let rec loop (s, pos) =
    match until (s, pos) with
    | Parsed (until_r, (s, new_p)) ->
        Parsed ((String.slice ~first:beg ~last:pos s, until_r), (s, new_p))
    | Failed ->
        (match p (s, pos) with
        | Parsed _ -> loop (s, succ pos)
        | Failed -> Failed) in
  loop (s, pos)

