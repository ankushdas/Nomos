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
module EM = ErrorMsg
open Sexplib.Std

type environment = (A.decl * A.ext) list
[@@deriving sexp]                 

type raw_transaction = RawTransaction of environment
[@@deriving sexp]
                                       
type transaction = Transaction of environment
[@@deriving sexp]                                

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

let ereset () = ErrorMsg.reset ()

let print_position lexbuf =
  let pos = lexbuf.L.lex_curr_p in
  pos.L.pos_fname ^ ":" ^
  string_of_int (pos.L.pos_lnum) ^ ":" ^
  string_of_int (pos.L.pos_cnum - pos.L.pos_bol + 1);;

(* lex and parse using Menhir, then return list of declarations *)
let parse_with_error lexbuf =
  try Parser.file Lexer.token lexbuf with
  | LEX.SyntaxError msg ->
      let lex_error = "Lexing Failure: " ^ print_position lexbuf ^ ": " ^ msg in
      raise (EM.LexError lex_error)
  | Parser.Error ->
      let parse_error = "Parsing Failure: " ^ print_position lexbuf in
      raise (EM.ParseError parse_error);;

(* try to read a file, or print failure *)
let read_with_error file =
  try C.In_channel.read_all file
  with Sys_error msg ->
    let errormsg = "Failed to load file:\n- " ^ msg ^ "\n" in
    raise (EM.FileError errormsg);;

(* open file and parse into environment *)
let read file =
  let rec read_env file =
    let () = ereset () in                         (* internal lexer and parser state *)
    (*
    let () = I.reset () in                        (* resets the LP solver *)
    *)
    let inx = read_with_error file in             (* read file *)
    let lexbuf = Lexing.from_string inx in        (* lex file *)
    let () = init lexbuf file in
    let (imports, env) = parse_with_error lexbuf in  (* parse file *)
    let imports' = List.map (Filename.concat (Filename.dirname file)) imports in
    let envs = List.map read_env imports' in
    (List.concat (envs @ [env]) : environment)
  in
    RawTransaction (read_env file)

let read_txn txn =
  let () = ereset () in
  let lexbuf = Lexing.from_string txn in
  let () = init lexbuf "txn.nom" in
  let (_imports, env) = parse_with_error lexbuf in
  RawTransaction env;;

(* check validity and typecheck environment *)
let infer state (RawTransaction decls) =
  let (_tx, _ch, _gas_accs, config_env, _config) = state in
  let config_env_with_ext = List.map (fun d -> (d, None)) config_env in
  let decls = config_env_with_ext @ decls in
  let t0 = Unix.gettimeofday () in
  let () = EL.check_redecl decls in           (* may raise ErrorMsg.Error *)
  let () = EL.check_valid decls decls in
  (* pragmas apply only to type-checker and execution *)
  (* may only be at beginning of file; apply now *)
  let decls' = EL.commit_channels decls decls in
  (* allow for mutually recursive definitions in the same file *)
  let env = EL.elab_decls decls' decls' in
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

let create_account txnsender initial_config =
  let (tx, ch, gas_accs, types, config) = initial_config in
  let new_gas_accs = G.create_account gas_accs txnsender in
  (tx, ch, new_gas_accs, types, config);;

let deposit_gas txnsender d initial_config =
  let (tx, ch, gas_accs, types, config) = initial_config in
  let new_gas_accs = G.deposit gas_accs txnsender d in
  (tx, ch, new_gas_accs, types, config);;

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
      | (A.TpDef _ as dcl, _ext)::dcls'
      | (A.ExpDecDef _ as dcl, _ext)::dcls' ->
          let (tx, ch, ga, env, conf) = config in
          run' (tx, ch, ga, dcl::env, conf) dcls'
      | [] -> config
  in
  let leftover = E.leftover_gas () in
  (run' config env, leftover)

(********************)
(* Interactive Mode *)
(********************)

let gconfig = ref E.empty_blockchain_state

let reset () = gconfig := E.empty_blockchain_state

let load path = gconfig := load_config path

let save path = save_config !gconfig path

let set_sender sender = E.txnSender := sender

let exec env =
  let (new_config, _leftover) = run env !gconfig in
  gconfig := new_config;;

let read_and_exec path = read path |> infer !gconfig |> exec

let show_channels () =
  let (_, _, _, _, {E.types = types; _}) = !gconfig in
  Map.iteri types ~f:(fun ~key:k ~data:v -> C.printf "%s: %s\n" (PP.pp_chan k) (PP.pp_tp_simple v))
