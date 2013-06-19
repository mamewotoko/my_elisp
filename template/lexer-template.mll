(* Header of lexer
{
open Lexing
open Parser        (* The type token is defined in parser.mli *)
exception Eof

let newline lexbuf =
  lexbuf.lex_curr_p <-
    { lexbuf.lex_curr_p with pos_lnum = lexbuf.lex_curr_p.pos_lnum + 1 }

let reset_lexer lexbuf =
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_lnum = 1 }

}
*)
let nl = '\013' '\010' | '\010'

  rule token = parse
  [' ' '\t' ]     { token lexbuf }     (* skip blanks *)
| nl             { newline lexbuf; token lexbuf }
| eof            { raise Eof }
    
(* trailer of lexer
{
}
*)
