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
module LEX = Lexer
module L = Lexing
module G = GasAcct

type environment = (A.decl * A.ext) list

type raw_transaction = RawTransaction of environment
type transaction = Transaction of environment

(*********************************)
(* Loading and Elaborating Files *)
(*********************************)

let init (lexbuf : Lexing.lexbuf) (fname : string) : unit =
  lexbuf.lex_curr_p <- {
    pos_fname = fname;
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = 0;
  }

let reset () = ErrorMsg.reset ()

let print_position _outx lexbuf =
  let pos = lexbuf.L.lex_curr_p in
  C.printf "%s:%d:%d" pos.L.pos_fname
    pos.L.pos_lnum (pos.L.pos_cnum - pos.L.pos_bol + 1)

(* lex and parse using Menhir, then return list of declarations *)
let parse_with_error lexbuf =
  try Parser.file Lexer.token lexbuf with
  | LEX.SyntaxError msg ->
      (C.printf "LEXING FAILURE: %a: %s\n" print_position lexbuf msg;
      exit 1)
  | Parser.Error ->
      (C.printf "PARSING FAILURE: %a\n" print_position lexbuf; exit 1)

(* try to read a file, or print failure *)
let read_with_error file =
  try C.In_channel.read_all file with
  | Sys_error s -> (C.eprintf "Failed to load file:\n- %s\n" s; exit 1)

(* open file and parse into environment *)
let read file =
  let rec read_env file =
    let () = reset () in                          (* internal lexer and parser state *)
    (*
    let () = I.reset () in                        (* resets the LP solver *)
    *)
    let inx = read_with_error file in             (* read file *)
    let lexbuf = Lexing.from_string inx in        (* lex file *)
    let _ = init lexbuf file in
    let (imports, (env, _ext)) = parse_with_error lexbuf in  (* parse file *)
    let imports' = List.map (Filename.concat (Filename.dirname file)) imports in
    let envs = List.map read_env imports' in
    (List.concat (envs @ [env]) : environment)
  in
    RawTransaction (read_env file)

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
  let () = if !F.verbosity >= 1 then print_string (StdList.fold_left (fun str (dcl, _) -> str ^ (PP.pp_decl env dcl) ^ "\n") "" env) in
  let () = if !F.verbosity >= 0 then print_string ("TC time: " ^ string_of_float (1000. *. (t1 -. t0)) ^ "\n") in
  let () = if !F.verbosity >= 0 then print_string ("Inference time: " ^ string_of_float (1000. *. (t2 -. t1)) ^ "\n") in
  let () = if !F.verbosity >= 0 then I.print_stats () in
  Transaction env

(**********************)
(* Executing Programs *)
(**********************)

let load_config config_in =
  Sexp.load_sexp_conv_exn config_in E.blockchain_state_of_sexp

let save_config conf config_out =
  Sexp.save_hum config_out (E.sexp_of_blockchain_state conf)

let create_account initial_config =
  G.create_account !E.txnSender initial_config;;

let deposit_gas d initial_config =
  G.deposit_gas !E.txnSender d initial_config;;

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
          let (tx, ch, ga, types, conf) = config in
          run' (tx, ch, ga, Map.set types ~key:n ~data:t, conf) dcls'
      | _dcl::dcls' -> run' config dcls'
      | [] -> config
  in
    run' config env

(********************)
(* Interactive Mode *)
(********************)

let gconfig = ref E.empty_blockchain_state

let reset () = gconfig := E.empty_blockchain_state

let load path = gconfig := load_config path

let save path = save_config !gconfig path

let set_sender sender = E.txnSender := sender

let exec env = gconfig := run env !gconfig

let read_and_exec path = read path |> infer |> exec

let show_channels () =
  let (_, _, _, _, {E.types = types; _}) = !gconfig in
  C.Map.iteri types ~f:(fun ~key:k ~data:v -> C.printf "%s: %s\n" (PP.pp_chan k) (PP.pp_tp_simple v))
