open Core
open Lexer
open Lexing


module P = Print
module TC = Typecheck
module E = Evaluate


let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  printf "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.file Lexer.token lexbuf with
  | SyntaxError msg ->
     (Printf.printf "LEXING FAILURE: %a: %s\n" print_position lexbuf msg; None)
  | Parser.Error ->
     (Printf.printf "PARSING FAILURE: %a\n" print_position lexbuf; None)


let rec process (l : Ast.program list) =
        match l with
                [] -> ()
        | Ast.Program(expr, t)::es -> try
                                      let a : bool = TC.typecheck [] expr t in       
                                      (if a then let evalRes = E.evaluate [] expr in Printf.printf "%s\n" (P.print_value evalRes)
                                      else Printf.printf "TYPECHECKING FAILURE\n"; process es)
                                      with
                                      | TC.TypeError err -> (Printf.printf "TYPECHECKING FAILURE: %s\n" err; process es)

(* part 1 *)
let rec parse_and_print lexbuf =
  match parse_with_error lexbuf with
  | Some (Ast.PL l) -> (process l; Printf.printf "DONE\n")
  | None -> ()

let () =
  let inx = In_channel.read_all "./test.ml" in
  let lexbuf = Lexing.from_string inx in
  parse_and_print lexbuf

