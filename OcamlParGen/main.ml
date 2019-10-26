open Core
open Lexer
open Lexing


module P = Print
module TC = Typecheck
module E = Evaluate
module I = Infer
module A = Ast
module U = Unify

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
        | Ast.Program(expr)::es -> try
                                      let _ = I.reset () in
                                      let temp = I.fresh () in
                                      let _ = Printf.printf "Expression: \n%s\n" (P.print_ast expr) in
                                      let l = I.unify_exp [] expr temp in
                                      let s = (U.unify l) in
                                      let t1 = U.find_type s in
                                      let res = U.find_res s in
                                      let t2 = U.apply res t1 in
                                      let _ = Printf.printf "Type: \n%s\n" (P.print_type t2) in
                                      let evalRes = E.evaluate [] expr in
                                      (Printf.printf "Value: \n%s\n\n" (P.print_value evalRes); process es)
                                      with
                                      | TC.TypeError err -> (Printf.printf "TYPECHECKING FAILURE: %s\n" err; process es)
                                      | E.EvaluationError err -> (Printf.printf "EVALUATION FAILURE: %s\n" err; process es)

(* part 1 *)
let rec parse_and_print lexbuf =
  match parse_with_error lexbuf with
  | Some (Ast.PL l) -> (process l; Printf.printf "DONE\n")
  | None -> ()

let () =
  let inx = In_channel.read_all "./test.ml" in
  let lexbuf = Lexing.from_string inx in
  parse_and_print lexbuf

