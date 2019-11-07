open Core
open Lexer
open Lexing

module A = Ast
module PP = Pprint

let print_position _outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  printf "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.file Lexer.token lexbuf with
  | SyntaxError msg ->
     (Printf.printf "LEXING FAILURE: %a: %s\n" print_position lexbuf msg; [])
  | Parser.Error ->
     (Printf.printf "PARSING FAILURE: %a\n" print_position lexbuf; [])

(* part 1 *)
let parse_and_print lexbuf =
  let env = parse_with_error lexbuf in
  print_string (PP.pp_program env env);; 

let () =
  let inx = In_channel.read_all "./test.nom" in
  let lexbuf = Lexing.from_string inx in
  parse_and_print lexbuf

