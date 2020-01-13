(* Configuration for running rast files *)
 
module R = Arith
module A = Ast
module PP = Pprint
module F = Flags
module C = Core
module EL = Elab
module I = Infer
module TC = Typecheck
open Lexer
open Lexing
 
let init (lexbuf : Lexing.lexbuf) (fname : string) : unit =
  let open Lexing in
  lexbuf.lex_curr_p <- {
    pos_fname = fname;
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = 0;
  }
  ;

(************************)
(* Command Line Options *)
(************************)
 
type option =
    Work of string
  | Syntax of string
  | Verbose of int;;

let print_position _outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  C.printf "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

(* try lexing and parsing *)
let parse_with_error lexbuf =
  try Parser.file Lexer.token lexbuf with
  | SyntaxError msg ->
      (C.printf "LEXING FAILURE: %a: %s\n" print_position lexbuf msg; 
      ([], None))
  | Parser.Error ->
      (C.printf "PARSING FAILURE: %a\n" print_position lexbuf; ([], None))

(* use the parser created by Menhir and return the list of declarations *)
let parse lexbuf =
  let env = parse_with_error lexbuf in
  env;; 
  

let process_option _ext op = match op with
    Work(s) ->
      begin
        match F.parseCost s with
            None -> C.eprintf "%% cost model %s not recognized\n" s; exit 1
          | Some cm -> F.work := cm
      end
  | Syntax(s) ->
      begin
        match F.parseSyntax s with
            None -> C.eprintf "%% syntax %s not recognized\n" s; exit 1
          | Some syn -> F.syntax := syn
      end
  | Verbose(level) -> F.verbosity := level;;

(*********************************)
(* Loading and Elaborating Files *)
(*********************************)
 
let reset () =
  ErrorMsg.reset ();;



let load file =
  let () = reset () in                        (* internal lexer and parser state *)
  (*
  let () = I.reset () in                      (* resets the LP solver *)
  *)
  let t0 = Unix.gettimeofday () in
  let inx = C.In_channel.read_all file in       (* read file *)
  let lexbuf = Lexing.from_string inx in      (* lex file *)
  let _ = init lexbuf file in
  let (decls, ext) = parse lexbuf in                 (* parse file *)
  let () = EL.check_redecl [] decls in        (* may raise ErrorMsg.Error *)
  (* pragmas apply only to type-checker and execution *)
  (* may only be at beginning of file; apply now *)
  let decls' = EL.commit_channels decls decls in
  (* allow for mutually recursive definitions in the same file *)
  let env = match EL.elab_decls decls' decls' ext with
                Some env' -> env'
              | None -> raise ErrorMsg.Error  (* error during elaboration *)
  in
  let t1 = Unix.gettimeofday () in
  let env = EL.remove_stars env in
  let env = EL.removeU env in
  let () = if !Flags.verbosity >= 2 then print_string ("========================================================\n") in
  let () = if !Flags.verbosity >= 2 then print_string (List.fold_left (fun str dcl -> str ^ (PP.pp_decl env dcl) ^ "\n") "" 
  (List.map (fun (x,_) -> x) env)) in
  let () = EL.gen_constraints env (List.map (fun (x,_) -> x) env) ext in
  let (psols,msols) = I.solve_and_print () in
  let env = EL.substitute (List.map (fun (x,_) -> x) env) psols msols in
  let t2 = Unix.gettimeofday () in
  let () = if !Flags.verbosity >= 1 then print_string ("========================================================\n") in
  let () = if !Flags.verbosity >= 0 then print_string (List.fold_left (fun str dcl -> str ^ (PP.pp_decl env dcl) ^ "\n") "" env) in
  let () = print_string ("TC time: " ^ string_of_float (1000. *. (t1 -. t0)) ^ "\n") in
  let () = print_string ("Inference time: " ^ string_of_float (1000. *. (t2 -. t1)) ^ "\n") in
  let () = I.print_stats () in
  env;;


(**********************)
(* Executing Programs *)
(**********************)

let run env dcls =
  match dcls with
      {A.declaration = A.Exec(f) ; A.decl_extent = _ext}::dcls' ->
        let () = if !Flags.verbosity >= 1
                 then print_string (PP.pp_decl env (A.Exec(f)) ^ "\n")
                 else () in
        let _config = E.exec env f in
        (* may raise Exec.RuntimeError *)
        run env dcls'
    | _dcl::dcls' -> run env dcls'
    | [] -> ();;


let cmd_ext = None;;

let nomos_file =
  C.Command.Arg_type.create
    (fun filename ->
      match C.Sys.is_file filename with
          `No | `Unknown ->
            begin
              C.eprintf "'%s' is not a regular file.\n%!" filename;
              exit 1
            end
        | `Yes ->
            if Filename.check_suffix filename ".nom"
            then filename
            else
              begin
                C.eprintf "'%s' does not have nom extension.\n%!" filename;
                exit 1
              end);;

let nomos_command =
  C.Command.basic
    ~summary:"Typechecking Nomos files"
    ~readme:(fun () -> "More detailed information")
    C.Command.Let_syntax.(
      let%map_open
        verbosity_flag = flag "-v" (optional int)
          ~doc:"verbosity 0: quiet, 1: default, 2: verbose, 3: debugging mode"
        and work_flag = flag "-w" (optional string)
          ~doc:"work-cost-model: none, recv, send, recvsend, free"
        and syntax_flag = flag "-s" (optional string)
          ~doc:"syntax: implicit, explicit"
        and file = anon("filename" %: nomos_file) in
        fun () ->
          let vlevel =
            begin
              match verbosity_flag with
                  None -> Verbose(1)
                | Some n -> Verbose(n)
            end
          in
          let work_cm =
            begin
              match work_flag with
                  None -> Work("none")
                | Some s -> Work(s)
            end
          in
          let syntax =
            begin
              match syntax_flag with
                  None -> Syntax("explicit")
                | Some s -> Syntax(s)
            end
          in
          let () = F.reset () in
          let () = List.iter (process_option cmd_ext) [vlevel; work_cm; syntax] in
          let env = try load file
                    with ErrorMsg.Error -> C.eprintf "%% compilation failed!\n"; exit 1 
          in
          let () = print_string ("% compilation successful!\n") in
          
          let () = run env env in
          print_string ("% runtime successful!\n"));;