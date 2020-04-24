module A = Ast
module C = Core
module E = Exec
module EL = Elab
module F = NomosFlags
module I = Infer
module Map = C.Map
module PP = Pprint
module R = Arith
module Sexp = C.Sexp
module TC = Typecheck
module StdList = Stdlib.List
open Lexer
open Lexing

type environment = (A.decl * A.ext) list

type raw_transaction = RawTransaction of environment
type transaction = Transaction of environment

(*********************************)
(* Loading and Elaborating Files *)
(*********************************)

let init (lexbuf : Lexing.lexbuf) (fname : string) : unit =
  let open Lexing in
  lexbuf.lex_curr_p <- {
    pos_fname = fname;
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = 0;
  }

let reset () = ErrorMsg.reset ()

let print_position _outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  C.printf "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

(* try lexing and parsing *)
let parse_with_error lexbuf =
  try Parser.file Lexer.token lexbuf with
  | SyntaxError msg ->
      (C.printf "LEXING FAILURE: %a: %s\n" print_position lexbuf msg;
      exit 1)
  | Parser.Error ->
      (C.printf "PARSING FAILURE: %a\n" print_position lexbuf; exit 1)

(* use the parser created by Menhir and return the list of declarations *)
let parse lexbuf =
  let env = parse_with_error lexbuf in
  env;;

(* open file and parse into environment *)
let read file =
  let () = reset () in                        (* internal lexer and parser state *)
  (*
  let () = I.reset () in                      (* resets the LP solver *)
  *)
  let inx = C.In_channel.read_all file in     (* read file *)
  let lexbuf = Lexing.from_string inx in      (* lex file *)
  let _ = init lexbuf file in
  let (env, _ext) = parse lexbuf in           (* parse file *)
  env

let build envs = RawTransaction (List.concat envs)

(* check validity and typecheck environment *)
let infer (RawTransaction decls) =
  let t0 = Unix.gettimeofday () in
  let () = EL.check_redecl decls in           (* may raise ErrorMsg.Error *)
  let () = EL.check_valid decls decls in
  (* pragmas apply only to type-checker and execution *)
  (* may only be at beginning of file; apply now *)
  let decls' = EL.commit_channels decls decls in
  (* allow for mutually recursive definitions in the same file *)
  let env = match EL.elab_decls decls' decls' with
                Some env' -> env'
              | None -> raise ErrorMsg.Error  (* error during elaboration *)
  in
  let t1 = Unix.gettimeofday () in
  let env = EL.remove_stars env in
  let env = EL.removeU env in
  let () = if !F.verbosity >= 2 then print_string ("========================================================\n") in
  let () = if !F.verbosity >= 2 then print_string (StdList.fold_left (fun str dcl -> str ^ (PP.pp_decl env dcl) ^ "\n") ""
    (StdList.map (fun (x,_) -> x) env)) in
  let () = EL.gen_constraints env env in
  let (psols,msols) = I.solve_and_print () in
  let env = EL.substitute env psols msols in
  let t2 = Unix.gettimeofday () in
  let () = if !F.verbosity >= 1 then print_string ("========================================================\n") in
  let () = if !F.verbosity >= 0 then print_string (StdList.fold_left (fun str (dcl, _) -> str ^ (PP.pp_decl env dcl) ^ "\n") "" env) in
  let () = print_string ("TC time: " ^ string_of_float (1000. *. (t1 -. t0)) ^ "\n") in
  let () = print_string ("Inference time: " ^ string_of_float (1000. *. (t2 -. t1)) ^ "\n") in
  let () = I.print_stats () in
  Transaction env

(**********************)
(* Executing Programs *)
(**********************)

let load_config config_in =
  Sexp.load_sexp_conv_exn config_in E.full_configuration_of_sexp

let save_config conf config_out =
  Sexp.save_hum config_out (E.sexp_of_full_configuration conf)

let run (Transaction env) config =
  let rec run' config dcls =
    match dcls with
        (A.Exec(f), _ext)::dcls' ->
          let () = if !F.verbosity >= 1
                   then print_string (PP.pp_decl env (A.Exec(f)) ^ "\n")
                   else () in
          let config' = E.exec env config (f, []) in
          (* may raise Exec.RuntimeError *)
          run' config' dcls'
      | (A.TpDef(n, t), _ext)::dcls' ->
          let (tx, ch, types, conf) = config in
          run' (tx, ch, Map.set types ~key:n ~data:t, conf) dcls'
      | _dcl::dcls' -> run' config dcls'
      | [] -> config
  in
    run' config env

(********************)
(* Interactive Mode *)
(********************)

let gconfig = ref E.empty_full_configuration

let reset () = gconfig := E.empty_full_configuration

let load path = gconfig := load_config path

let save path = save_config !gconfig path

let exec env = gconfig := run env !gconfig

let load_and_exec paths =
  C.List.map ~f:read paths
  |> build
  |> infer
  |> exec
