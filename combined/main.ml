open Core
open Lexer
open Lexing


module A = Ast

let print_position outx lexbuf =
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
let rec parse_and_print lexbuf =
  let _ = parse_with_error lexbuf in
  print_string ("DONE\n")

let () =
  let inx = In_channel.read_all "./test.ml" in
  let lexbuf = Lexing.from_string inx in
  parse_and_print lexbuf

