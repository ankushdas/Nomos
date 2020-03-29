(* Nomos command-line tool entry point *)

module R = Arith
module A = Ast
module PP = Pprint
module C = Core
module EL = Elab
module I = Infer
module TC = Typecheck
module F = NomosFlags
module E = Exec
open Lexer
open Lexing

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
  parse lexbuf;;                              (* parse file *)

(* check validity and typecheck environment *)
let validate decls =
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
  let () = if !F.verbosity >= 2 then print_string (List.fold_left (fun str dcl -> str ^ (PP.pp_decl env dcl) ^ "\n") ""
    (List.map (fun (x,_) -> x) env)) in
  let () = EL.gen_constraints env env in
  let (psols,msols) = I.solve_and_print () in
  let env = EL.substitute env psols msols in
  let t2 = Unix.gettimeofday () in
  let () = if !F.verbosity >= 1 then print_string ("========================================================\n") in
  let () = if !F.verbosity >= 0 then print_string (List.fold_left (fun str (dcl, _) -> str ^ (PP.pp_decl env dcl) ^ "\n") "" env) in
  let () = print_string ("TC time: " ^ string_of_float (1000. *. (t1 -. t0)) ^ "\n") in
  let () = print_string ("Inference time: " ^ string_of_float (1000. *. (t2 -. t1)) ^ "\n") in
  let () = I.print_stats () in
  env;;  


(**********************)
(* Executing Programs *)
(**********************)

let load_config config_in =
  match config_in with
  | None -> E.empty_full_configuration
  | Some(path) -> C.Sexp.load_sexp_conv_exn path E.full_configuration_of_sexp

let rec run env config dcls =
  match dcls with
      (A.Exec(f), _ext)::dcls' ->
        let () = if !F.verbosity >= 1
                 then print_string (PP.pp_decl env (A.Exec(f)) ^ "\n")
                 else () in
        let config' = E.exec env config (f, []) in
        (* may raise Exec.RuntimeError *)
        run env config' dcls'
    | _dcl::dcls' -> run env config dcls'
    | [] -> config;;

(************************)
(* Command Line Options *)
(************************)

let set_cost_model s =
  match F.parseCost s with
      None -> (C.eprintf "%% cost model %s not recognized\n" s; exit 1)
    | Some cm -> F.work := cm

let set_syntax s =
  match F.parseSyntax s with
      None -> (C.eprintf "%% syntax %s not recognized\n" s; exit 1)
    | Some syn -> F.syntax := syn

let check_extension filename ext =
  if Filename.check_suffix filename ext
  then filename
  else
    begin
      C.eprintf "'%s' does not have %s extension.\n%!" filename ext;
      exit 1
    end
    
let file (ext : string) =
  C.Command.Arg_type.create
    (fun filename ->
      match C.Sys.is_file filename with
          `No | `Unknown ->
            begin
              C.eprintf "'%s' is not a regular file.\n%!" filename;
              exit 1
            end
        | `Yes -> check_extension filename ext)

let nomos_file = file ".nom"

let in_conf_file = file ".conf"

let out_conf_file =
  C.Command.Arg_type.create
    (fun filename -> check_extension filename ".conf")

let txn_file = file ".txn"

let nomos_command =
  C.Command.basic
    ~summary:"Typechecking and Executing Nomos files"
    C.Command.Let_syntax.(
      let%map_open
        verbosity = flag "-v" (optional_with_default 1 int)
          ~doc:"verbosity 0: quiet, 1: default, 2: verbose, 3: debugging mode"
        and cost_model = flag "-w" (optional_with_default "none" string)
          ~doc:"work-cost-model none, recv, send, recvsend, free"
        and syntax = flag "-s" (optional_with_default "explicit" string)
          ~doc:"syntax implicit, explicit"
        and tc_only = flag "-tc" no_arg
          ~doc:"tc only"
        and config_in = flag "-i" (optional in_conf_file)
          ~doc:"input configuration path"
        and config_out = flag "-o" (optional out_conf_file)
          ~doc:"output configuration path"
        and txn_path = flag "-t" (optional txn_file)
          ~doc:"transaction file path"
        and file = anon("filename" %: nomos_file) in
        fun () ->
          (* set global flags *)
          let () = F.reset () in
          let () = F.verbosity := verbosity in
          let () = set_cost_model cost_model in
          let () = set_syntax syntax in
          let () =
            if tc_only && List.exists Option.is_some [config_in; config_out; txn_path]
              then
                begin
                  C.eprintf "cannot use execution options with -tc flag";
                  exit 1
                end
              else
                () in

          (* parse *)
          let (contract_env,_ext) = read file in
          let () = print_string ("% contract parsing successful!\n") in
          let (txn_env, _ext) =
            match txn_path with
                None -> ([], None)
              | Some(txn_file) -> 
                  let e = read txn_file in
                  begin
                    print_string "% transaction parsing successful!\n";
                    e
                  end
          in

          (* typecheck *)
          let env = try validate (contract_env @ txn_env)
                    with ErrorMsg.Error -> C.eprintf "%% compilation failed!\n"; exit 1
          in
          let () = print_string ("% compilation successful!\n") in

          if tc_only
            then ()
            else
              (* run transaction *)
              let initial_config = load_config config_in in
              let final_config = run env initial_config env in
              let () = print_string ("% runtime successful!\n") in

              (* save final configuration *)
              match config_out with
                  None -> ()
                | Some(path) -> C.Sexp.save path (E.sexp_of_full_configuration final_config));;
