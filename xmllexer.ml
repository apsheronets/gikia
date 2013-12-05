# 27 "xmllexer.mll"
 

type lexing_error =
    | EUnterminatedComment
    | EUnterminatedString
    | EIdentExpected
    | ECloseExpected
    | ENodeExpected
    | EAttributeNameExpected
    | EAttributeValueExpected
    | EUnterminatedEntity
    | ECamlIdentExpected
    | WNestedComments


let lex_error_to_string = function
  | EUnterminatedComment -> "Unterminated Comment"
  | EUnterminatedString  -> "Unterminated String"
  | EIdentExpected -> "Ident Expected"
  | ECloseExpected -> "Close Expected"
  | ENodeExpected -> "Node Expected"
  | EAttributeNameExpected -> "Attribute Name Expected"
  | EAttributeValueExpected -> "Attribute Value Expected"
  | EUnterminatedEntity -> "Unterminated Entity"
  | ECamlIdentExpected -> "Caml Ident Expected"
  | WNestedComments -> "Nested comments (non valid XML)"



open Camlp4
module Loc = Camlp4.PreCast.Loc
open Lexing


(* To store some context information:
     *   loc    : position of the beginning of a string, quotation and comment
     *   entity : shall we translate entities or not
*)

type context = {
  loc        : Loc.t    ;
  lexbuf     : lexbuf   ;
  buffer     : Buffer.t ;
  entity     : bool ;
}

let default_context lb = {
  loc        = Loc.ghost ;
  lexbuf     = lb        ;
  buffer     = Buffer.create 256 ;
  entity     = true ;
}

(* To buffer string literals *)

let store c = Buffer.add_string c.buffer (Lexing.lexeme c.lexbuf)
let istore_char c i = Buffer.add_char c.buffer (Lexing.lexeme_char c.lexbuf i)
let buff_contents c =
        let contents = Buffer.contents c.buffer in
        Buffer.reset c.buffer; contents

let loc c = Loc.merge c.loc (Loc.of_lexbuf c.lexbuf)

let with_curr_loc f c = f { (c) with loc = Loc.of_lexbuf c.lexbuf } c.lexbuf
let parse f c = f c c.lexbuf

let lexeme c = Lexing.lexeme c.lexbuf
let store_string c s = Buffer.add_string c.buffer s


(* Deal with entities  *)
let idents = Hashtbl.create 0

let _ = begin
        Hashtbl.add idents "gt;" ">";
        Hashtbl.add idents "lt;" "<";
        Hashtbl.add idents "amp;" "&";
        Hashtbl.add idents "apos;" "'";
        Hashtbl.add idents "quot;" "\"";
end


(* Update the current location with file name and line number. *)
let update_loc c line absolute chars =
        let lexbuf = c.lexbuf in
        let pos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <- { pos with
        pos_lnum = if absolute then line else pos.pos_lnum + line;
        pos_bol = pos.pos_cnum - chars;
        }


(* The lexer is parameterized to handle both Xml and Xhtml with
   inlined Caml code modes. *)
module type LexerArgSig = sig
  (* We do not raise the same exception in both lexers *)
  val error : Camlp4.PreCast.Loc.t -> lexing_error -> exn

  type attr_name =  private [> `AttrName of string ]
  type attr_value =  private [> `AttrVal of string ]
  type attribute = private [> `Attribute of (attr_name * attr_value) ]

  type token = private [>
  | `Tag of (string * (attribute list) * bool)
  | `PCData of string
  | `Endtag of string
  | `Comment of string
  | `Whitespace of string
  | `Eof
  ]

  val parse_dollar_token :     context ->          lexbuf -> token
  val parse_dollar_attrname :  context -> Loc.t -> lexbuf -> attr_name
  val parse_dollar_attrvalue : context -> Loc.t -> lexbuf -> attr_value
  val parse_dollar_attribute : context -> Loc.t -> lexbuf -> attribute

end


module Make (X : LexerArgSig) = struct

let err error loc =
  raise (X.error loc error)



# 129 "xmllexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\244\255\245\255\001\000\247\255\000\000\008\000\010\000\
    \012\000\255\255\011\000\017\000\252\255\000\000\001\000\253\255\
    \249\255\020\000\015\000\255\255\006\000\251\255\252\255\008\000\
    \001\000\255\255\009\000\011\000\254\255\003\000\253\255\005\000\
    \252\255\253\255\005\000\255\255\254\255\060\000\021\000\023\000\
    \065\000\254\255\252\255\063\000\254\255\127\000\255\255\225\000\
    \047\001\254\255\125\001\007\000\254\255\255\255\040\000\000\000\
    \015\000\255\255\254\255\000\000\000\000\000\000\001\000\253\255\
    \212\001\253\255\254\255\034\002\000\001\252\255\075\000\001\001\
    \085\000\242\001\253\255\254\255\003\000\007\000\012\000\021\000\
    \255\255\063\000\252\255\253\255\065\000\255\255\254\255\184\000\
    \252\255\253\255\067\000\255\255\254\255";
  Lexing.lex_backtrk = 
   "\255\255\255\255\255\255\009\000\255\255\007\000\005\000\001\000\
    \001\000\255\255\004\000\005\000\255\255\255\255\255\255\255\255\
    \255\255\002\000\001\000\255\255\255\255\255\255\255\255\004\000\
    \004\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\003\000\255\255\255\255\004\000\255\255\002\000\
    \000\000\255\255\255\255\255\255\255\255\001\000\255\255\255\255\
    \255\255\255\255\000\000\255\255\255\255\255\255\003\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\000\000\255\255\255\255\003\000\003\000\
    \255\255\255\255\255\255\255\255\000\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\003\000\255\255\255\255\255\255\
    \255\255\255\255\003\000\255\255\255\255";
  Lexing.lex_default = 
   "\003\000\000\000\000\000\003\000\000\000\255\255\255\255\255\255\
    \003\000\000\000\255\255\255\255\000\000\255\255\255\255\000\000\
    \000\000\255\255\255\255\000\000\021\000\000\000\000\000\255\255\
    \255\255\000\000\255\255\255\255\000\000\255\255\000\000\032\000\
    \000\000\000\000\255\255\000\000\000\000\040\000\255\255\255\255\
    \040\000\000\000\000\000\044\000\000\000\255\255\000\000\255\255\
    \049\000\000\000\255\255\052\000\000\000\000\000\255\255\255\255\
    \255\255\000\000\000\000\255\255\255\255\255\255\255\255\000\000\
    \065\000\000\000\000\000\255\255\069\000\000\000\255\255\255\255\
    \255\255\255\255\000\000\000\000\255\255\255\255\255\255\255\255\
    \000\000\082\000\000\000\000\000\255\255\000\000\000\000\088\000\
    \000\000\000\000\255\255\000\000\000\000";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\008\000\009\000\255\255\000\000\007\000\255\255\035\000\
    \025\000\011\000\000\000\007\000\010\000\008\000\255\255\007\000\
    \018\000\007\000\011\000\000\000\018\000\018\000\019\000\000\000\
    \008\000\018\000\026\000\016\000\004\000\255\255\005\000\255\255\
    \011\000\013\000\007\000\010\000\008\000\014\000\015\000\018\000\
    \255\255\011\000\255\255\023\000\018\000\029\000\027\000\010\000\
    \028\000\042\000\041\000\063\000\006\000\255\255\001\000\255\255\
    \010\000\030\000\024\000\036\000\034\000\053\000\255\255\012\000\
    \255\255\255\255\255\255\255\255\055\000\058\000\255\255\080\000\
    \000\000\000\000\000\000\000\000\072\000\000\000\000\000\056\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\072\000\000\000\
    \038\000\085\000\039\000\086\000\000\000\255\255\057\000\255\255\
    \000\000\060\000\092\000\072\000\059\000\075\000\000\000\076\000\
    \000\000\000\000\074\000\061\000\062\000\072\000\077\000\075\000\
    \255\255\076\000\255\255\078\000\074\000\255\255\079\000\255\255\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\000\000\084\000\000\000\086\000\000\000\092\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\046\000\000\000\000\000\000\000\000\000\000\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\000\000\000\000\000\000\000\000\000\000\091\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \002\000\255\255\000\000\000\000\000\000\033\000\022\000\000\000\
    \000\000\071\000\073\000\000\000\255\255\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\090\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\046\000\000\000\000\000\000\000\
    \071\000\073\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\255\255\070\000\072\000\083\000\
    \000\000\255\255\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\050\000\000\000\000\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\000\000\000\000\000\000\000\000\050\000\000\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\000\000\000\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \089\000\000\000\000\000\000\000\000\000\000\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \000\000\000\000\000\000\000\000\050\000\000\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \066\000\000\000\000\000\073\000\000\000\000\000\000\000\000\000\
    \000\000\067\000\000\000\000\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\000\000\
    \000\000\000\000\073\000\000\000\000\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\072\000\
    \000\000\000\000\000\000\067\000\000\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \000\000\000\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\000\000\000\000\000\000\
    \000\000\067\000\000\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\003\000\255\255\000\000\003\000\031\000\
    \020\000\006\000\255\255\007\000\010\000\008\000\008\000\007\000\
    \018\000\008\000\011\000\255\255\018\000\017\000\017\000\255\255\
    \000\000\017\000\024\000\005\000\000\000\003\000\000\000\003\000\
    \006\000\006\000\007\000\010\000\008\000\013\000\014\000\018\000\
    \008\000\011\000\008\000\020\000\017\000\023\000\026\000\006\000\
    \027\000\038\000\039\000\062\000\000\000\003\000\000\000\003\000\
    \011\000\029\000\020\000\034\000\031\000\051\000\037\000\006\000\
    \008\000\037\000\008\000\040\000\054\000\056\000\040\000\079\000\
    \255\255\255\255\255\255\255\255\070\000\255\255\255\255\054\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\072\000\255\255\
    \037\000\081\000\037\000\084\000\255\255\040\000\054\000\040\000\
    \255\255\059\000\090\000\070\000\055\000\070\000\255\255\070\000\
    \255\255\255\255\070\000\060\000\061\000\072\000\076\000\072\000\
    \037\000\072\000\037\000\077\000\072\000\040\000\078\000\040\000\
    \043\000\043\000\043\000\043\000\043\000\043\000\043\000\043\000\
    \043\000\043\000\043\000\043\000\043\000\043\000\043\000\043\000\
    \043\000\043\000\043\000\043\000\043\000\043\000\043\000\043\000\
    \043\000\043\000\255\255\081\000\255\255\084\000\255\255\090\000\
    \043\000\043\000\043\000\043\000\043\000\043\000\043\000\043\000\
    \043\000\043\000\043\000\043\000\043\000\043\000\043\000\043\000\
    \043\000\043\000\043\000\043\000\043\000\043\000\043\000\043\000\
    \043\000\043\000\045\000\255\255\255\255\255\255\255\255\255\255\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\255\255\255\255\255\255\255\255\255\255\087\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
    \045\000\045\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\003\000\255\255\255\255\255\255\031\000\020\000\255\255\
    \255\255\068\000\071\000\255\255\008\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\087\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\047\000\255\255\255\255\255\255\
    \068\000\071\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\037\000\068\000\071\000\081\000\
    \255\255\040\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\047\000\047\000\047\000\047\000\
    \047\000\047\000\047\000\047\000\048\000\255\255\255\255\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\255\255\255\255\255\255\255\255\048\000\255\255\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\048\000\048\000\048\000\048\000\048\000\048\000\
    \048\000\048\000\050\000\255\255\255\255\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \087\000\255\255\255\255\255\255\255\255\255\255\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \255\255\255\255\255\255\255\255\050\000\255\255\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \050\000\050\000\050\000\050\000\050\000\050\000\050\000\050\000\
    \064\000\255\255\255\255\073\000\255\255\255\255\255\255\255\255\
    \255\255\064\000\255\255\255\255\064\000\064\000\064\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\255\255\
    \255\255\255\255\073\000\255\255\255\255\064\000\064\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\073\000\
    \255\255\255\255\255\255\064\000\255\255\064\000\064\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\064\000\
    \064\000\064\000\064\000\064\000\064\000\064\000\064\000\067\000\
    \255\255\255\255\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\255\255\255\255\255\255\
    \255\255\067\000\255\255\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\067\000\067\000\067\000\
    \067\000\067\000\067\000\067\000\067\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255";
  Lexing.lex_base_code = 
   "";
  Lexing.lex_backtrk_code = 
   "";
  Lexing.lex_default_code = 
   "";
  Lexing.lex_trans_code = 
   "";
  Lexing.lex_check_code = 
   "";
  Lexing.lex_code = 
   "";
}

let rec token c lexbuf =
    __ocaml_lex_token_rec c lexbuf 0
and __ocaml_lex_token_rec c lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 167 "xmllexer.mll"
            ( update_loc c 1 false 0; `Whitespace (lexeme c) )
# 395 "xmllexer.ml"

  | 1 ->
# 168 "xmllexer.mll"
                    ( `Whitespace (lexeme c) )
# 400 "xmllexer.ml"

  | 2 ->
# 174 "xmllexer.mll"
           ( `Comment(comment c [Loc.of_lexbuf c.lexbuf] lexbuf))
# 405 "xmllexer.ml"

  | 3 ->
# 175 "xmllexer.mll"
         ( header c lexbuf;  token c lexbuf )
# 410 "xmllexer.ml"

  | 4 ->
# 176 "xmllexer.mll"
                          (
      let tag = ident_name c lexbuf in
      ignore_spaces c lexbuf;
      close_tag c lexbuf;
      `Endtag tag
    )
# 420 "xmllexer.ml"

  | 5 ->
# 182 "xmllexer.mll"
               (
      let tag = ident_name c lexbuf in
      ignore_spaces c lexbuf;
      let attribs, closed = attributes c lexbuf in
      `Tag(tag, attribs, closed)
    )
# 430 "xmllexer.ml"

  | 6 ->
# 188 "xmllexer.mll"
         (
      ignore(buff_contents c);
      store c ;
      `PCData (pcdata c lexbuf)
    )
# 439 "xmllexer.ml"

  | 7 ->
# 193 "xmllexer.mll"
        (
      ignore (buff_contents c);
      store_string c (entity c lexbuf);
      `PCData (pcdata c lexbuf)
    )
# 448 "xmllexer.ml"

  | 8 ->
# 198 "xmllexer.mll"
        (
      ignore (buff_contents c);
      X.parse_dollar_token c lexbuf
    )
# 456 "xmllexer.ml"

  | 9 ->
# 202 "xmllexer.mll"
                   (
      ignore (buff_contents c);
      store c ;
      `PCData (pcdata c lexbuf)
    )
# 465 "xmllexer.ml"

  | 10 ->
# 207 "xmllexer.mll"
        ( `Eof )
# 470 "xmllexer.ml"

  | 11 ->
# 208 "xmllexer.mll"
      ( err ENodeExpected (Loc.of_lexbuf lexbuf) )
# 475 "xmllexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_token_rec c lexbuf __ocaml_lex_state

and ignore_spaces c lexbuf =
    __ocaml_lex_ignore_spaces_rec c lexbuf 17
and __ocaml_lex_ignore_spaces_rec c lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 211 "xmllexer.mll"
            (
      update_loc c 1 false 0;
      ignore_spaces c lexbuf
    )
# 489 "xmllexer.ml"

  | 1 ->
# 215 "xmllexer.mll"
                      ( ignore_spaces c lexbuf )
# 494 "xmllexer.ml"

  | 2 ->
# 216 "xmllexer.mll"
       ( () )
# 499 "xmllexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_ignore_spaces_rec c lexbuf __ocaml_lex_state

and comment c l lexbuf =
    __ocaml_lex_comment_rec c l lexbuf 20
and __ocaml_lex_comment_rec c l lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 226 "xmllexer.mll"
            (
      update_loc c 1 false 0;
      comment c l lexbuf
    )
# 513 "xmllexer.ml"

  | 1 ->
# 230 "xmllexer.mll"
           (
(*    warn WNestedComments (Loc.of_lexbuf lexbuf) ; *)
      store_string c "<!--" ;
      ignore (comment c (loc c :: l) lexbuf);
      store_string c "-->" ;
      comment c l lexbuf
    )
# 524 "xmllexer.ml"

  | 2 ->
# 237 "xmllexer.mll"
          (
      match l with
        | [] (* Should not occur... *)
        | [_] -> buff_contents c
        | _ -> ""
    )
# 534 "xmllexer.ml"

  | 3 ->
# 243 "xmllexer.mll"
        (
      err EUnterminatedComment
        (match l with | [] -> Loc.of_lexbuf lexbuf
                      | l :: _ -> l)
    )
# 543 "xmllexer.ml"

  | 4 ->
# 248 "xmllexer.mll"
      ( store c ; comment c l lexbuf )
# 548 "xmllexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_comment_rec c l lexbuf __ocaml_lex_state

and header c lexbuf =
    __ocaml_lex_header_rec c lexbuf 31
and __ocaml_lex_header_rec c lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 252 "xmllexer.mll"
            (
      update_loc c 1 false 0;
      header c lexbuf
    )
# 562 "xmllexer.ml"

  | 1 ->
# 256 "xmllexer.mll"
         ( () )
# 567 "xmllexer.ml"

  | 2 ->
# 257 "xmllexer.mll"
        ( err ECloseExpected (loc c) )
# 572 "xmllexer.ml"

  | 3 ->
# 258 "xmllexer.mll"
      ( header c lexbuf )
# 577 "xmllexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_header_rec c lexbuf __ocaml_lex_state

and pcdata c lexbuf =
    __ocaml_lex_pcdata_rec c lexbuf 37
and __ocaml_lex_pcdata_rec c lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 261 "xmllexer.mll"
            (
      store c ;
      pcdata c lexbuf
    )
# 591 "xmllexer.ml"

  | 1 ->
# 265 "xmllexer.mll"
         (
      store c ;
      pcdata c lexbuf;
    )
# 599 "xmllexer.ml"

  | 2 ->
# 269 "xmllexer.mll"
        (
      store_string c (entity c lexbuf);
      pcdata c lexbuf
    )
# 607 "xmllexer.ml"

  | 3 ->
# 273 "xmllexer.mll"
         ( store_string c "$" ; pcdata c lexbuf )
# 612 "xmllexer.ml"

  | 4 ->
# 274 "xmllexer.mll"
       ( buff_contents c )
# 617 "xmllexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_pcdata_rec c lexbuf __ocaml_lex_state

and entity c lexbuf =
    __ocaml_lex_entity_rec c lexbuf 43
and __ocaml_lex_entity_rec c lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 277 "xmllexer.mll"
                    (
      let ident = lexeme c in
      if not c.entity then "&" ^ ident else
        try Hashtbl.find idents (String.lowercase ident)
        with Not_found -> "&" ^ ident
    )
# 633 "xmllexer.ml"

  | 1 ->
# 283 "xmllexer.mll"
            ( err EUnterminatedEntity (Loc.of_lexbuf lexbuf) )
# 638 "xmllexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_entity_rec c lexbuf __ocaml_lex_state

and ident_name c lexbuf =
    __ocaml_lex_ident_name_rec c lexbuf 48
and __ocaml_lex_ident_name_rec c lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 286 "xmllexer.mll"
               ( lexeme c )
# 649 "xmllexer.ml"

  | 1 ->
# 287 "xmllexer.mll"
            ( err EIdentExpected (loc c))
# 654 "xmllexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_ident_name_rec c lexbuf __ocaml_lex_state

and close_tag c lexbuf =
    __ocaml_lex_close_tag_rec c lexbuf 51
and __ocaml_lex_close_tag_rec c lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 290 "xmllexer.mll"
        ( () )
# 665 "xmllexer.ml"

  | 1 ->
# 291 "xmllexer.mll"
            ( err ECloseExpected (loc c))
# 670 "xmllexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_close_tag_rec c lexbuf __ocaml_lex_state

and attributes c lexbuf =
    __ocaml_lex_attributes_rec c lexbuf 54
and __ocaml_lex_attributes_rec c lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 294 "xmllexer.mll"
         ( [], false )
# 681 "xmllexer.ml"

  | 1 ->
# 295 "xmllexer.mll"
         ( [], true )
# 686 "xmllexer.ml"

  | 2 ->
# 296 "xmllexer.mll"
             (
      let attribute = X.parse_dollar_attribute c (Loc.of_lexbuf lexbuf) lexbuf in
      ignore_spaces c lexbuf;
      let others, closed = attributes c lexbuf in
      attribute :: others, closed
    )
# 696 "xmllexer.ml"

  | 3 ->
# 302 "xmllexer.mll"
        (
      let attribute =
        let a = attr_name c lexbuf in
        let data = attr_data c lexbuf in
        `Attribute (a, data)
      in
      ignore_spaces c lexbuf;
      let others, closed = attributes c lexbuf in
      attribute :: others, closed
    )
# 710 "xmllexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_attributes_rec c lexbuf __ocaml_lex_state

and attr_name c lexbuf =
    __ocaml_lex_attr_name_rec c lexbuf 64
and __ocaml_lex_attr_name_rec c lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 314 "xmllexer.mll"
               ( `AttrName (lexeme c) )
# 721 "xmllexer.ml"

  | 1 ->
# 316 "xmllexer.mll"
        (
      X.parse_dollar_attrname c (loc c) lexbuf
    )
# 728 "xmllexer.ml"

  | 2 ->
# 320 "xmllexer.mll"
            ( err EAttributeNameExpected (loc c))
# 733 "xmllexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_attr_name_rec c lexbuf __ocaml_lex_state

and attr_data c lexbuf =
    __ocaml_lex_attr_data_rec c lexbuf 68
and __ocaml_lex_attr_data_rec c lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 323 "xmllexer.mll"
                                      (
      ignore(buff_contents c) ;
      X.parse_dollar_attrvalue c (loc c) lexbuf
    )
# 747 "xmllexer.ml"

  | 1 ->
# 327 "xmllexer.mll"
                          (
      ignore(buff_contents c) ;
      `AttrVal(dq_string c lexbuf)
    )
# 755 "xmllexer.ml"

  | 2 ->
# 331 "xmllexer.mll"
                           (
      ignore(buff_contents c) ;
      `AttrVal(q_string c lexbuf)
    )
# 763 "xmllexer.ml"

  | 3 ->
# 335 "xmllexer.mll"
            ( err EAttributeValueExpected (loc c))
# 768 "xmllexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_attr_data_rec c lexbuf __ocaml_lex_state

and dq_string c lexbuf =
    __ocaml_lex_dq_string_rec c lexbuf 81
and __ocaml_lex_dq_string_rec c lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 338 "xmllexer.mll"
        ( buff_contents c )
# 779 "xmllexer.ml"

  | 1 ->
# 339 "xmllexer.mll"
                      (
      istore_char c 1;
      dq_string c lexbuf
    )
# 787 "xmllexer.ml"

  | 2 ->
# 343 "xmllexer.mll"
        ( err EUnterminatedString (Loc.of_lexbuf lexbuf) )
# 792 "xmllexer.ml"

  | 3 ->
# 344 "xmllexer.mll"
      (
      istore_char c 0;
      dq_string c lexbuf
    )
# 800 "xmllexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_dq_string_rec c lexbuf __ocaml_lex_state

and q_string c lexbuf =
    __ocaml_lex_q_string_rec c lexbuf 87
and __ocaml_lex_q_string_rec c lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 350 "xmllexer.mll"
         ( buff_contents c )
# 811 "xmllexer.ml"

  | 1 ->
# 351 "xmllexer.mll"
                       (
      istore_char c 1;
      q_string c lexbuf
    )
# 819 "xmllexer.ml"

  | 2 ->
# 355 "xmllexer.mll"
         ( err EUnterminatedString (Loc.of_lexbuf lexbuf) )
# 824 "xmllexer.ml"

  | 3 ->
# 356 "xmllexer.mll"
      (
      istore_char c 0;
      q_string c lexbuf
    )
# 832 "xmllexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_q_string_rec c lexbuf __ocaml_lex_state

;;

# 363 "xmllexer.mll"
 
let lexing_store s buff max =
        let rec self n s =
                if n >= max then n
                else
                        match Stream.peek s with
                        | Some x ->
                                        Stream.junk s;
                                        buff.[n] <- x;
                                        succ n
                        | _ -> n
                in
                self 0 s

let from_context c  =
        let next _ =
                let tok = with_curr_loc token c in
                let loc = Loc.of_lexbuf c.lexbuf in
                Some ((tok, loc))
                in Stream.from next

let from_lexbuf lb e =
        let c = { (default_context lb) with
        loc        = Loc.of_lexbuf lb;
                entity = e }
        in from_context c

let setup_loc lb loc =
        let start_pos = Loc.start_pos loc in
        lb.lex_abs_pos <- start_pos.pos_cnum;
        lb.lex_curr_p  <- start_pos

let from_string loc entity str =
        let lb = Lexing.from_string str in
        setup_loc lb loc;
        from_lexbuf lb entity

let from_stream loc entity strm =
        let lb = Lexing.from_function (lexing_store strm) in
        setup_loc lb loc;
        from_lexbuf lb entity

end


# 884 "xmllexer.ml"
