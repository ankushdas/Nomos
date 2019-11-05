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
  let l = parse_with_error lexbuf in
  if List.length l = 0
  then print_string("empty file\n")
  else print_string ("parsing completed\n")

let () =
  let inx = In_channel.read_all "./test.nom" in
  let lexbuf = Lexing.from_string inx in
  parse_and_print lexbuf

