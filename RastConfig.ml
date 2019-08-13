(* Top Level *)
 
module R = Arith
module A = Ast
module PP = Pprint
module F = Flags
module C = Core
 
(************************)
(* Command Line Options *)
(************************)
 
type option =
    Work of string
  | Syntax of string
  | Verbose of int;;

let process_option op = match op with
    Work(s) ->
      begin
        match F.parseCost s with
            None -> C.eprintf "cost model %s not recognized" s; exit 1
          | Some cm -> F.work := cm
      end
  | Syntax(s) ->
      begin
        match F.parseSyntax s with
            None -> C.eprintf "syntax %s not recognized" s; exit 1
          | Some syn -> F.syntax := syn
      end
  | Verbose(level) -> F.verbosity := level;;

(*********************************)
(* Loading and Elaborating Files *)
(*********************************)
 
let reset () =
  Parsestate.reset ()
  ; ErrorMsg.reset ();;

let rec apply_pragmas dcls = match dcls with
    {A.declaration = A.Pragma("#options",line); A.decl_extent = _ext}::dcls' ->
      if !F.verbosity >= 1
      then print_string ("#options" ^ line ^ "\n")
      else ()
      ; apply_pragmas dcls'
  | {A.declaration = A.Pragma("#test",_line); A.decl_extent = _ext}::dcls' ->
    (* ignore #test pragma *)
      apply_pragmas dcls'
  | {A.declaration = A.Pragma(pragma,_line); A.decl_extent = ext}::_dcls' ->
      ErrorMsg.error_msg ErrorMsg.Pragma ext ("unrecognized pragma: " ^ pragma)
      ; raise ErrorMsg.Error
  | dcls' -> dcls';;

let load file =
  let () = reset () in                        (* internal lexer and parser state *)
  let decls = Parse.parse file in             (* may raise ErrorMsg.Error *)
  let () = Elab.check_redecl [] decls in      (* may raise ErrorMsg.Error *)
  (* pragmas apply only to type-checker and execution *)
  (* may only be at beginning of file; apply now *)
  let decls' = Elab.commit_channels decls decls in
  let decls'' = apply_pragmas decls' in       (* remove pragmas; may raise ErrorMsg.Error *)
  (* allow for mutually recursive definitions in the same file *)
  match Elab.elab_decls decls'' decls'' with
      Some env' -> env'
    | None -> raise ErrorMsg.Error;;                    (* error during elaboration *)

(*
(**********************)
(* Executing Programs *)
(**********************)

fun init_pot env f =
  (case A.lookup_expdec env f
    of SOME(_,_,(_,pot,_)) => R.evaluate pot
    |  NONE => raise ErrorMsg.Error) 

(* measure cost = true if 'cost' is measured *)
fun measure Flags.None = false
  | measure _ = true

fun run env (A.Exec(f,ext)::decls) =
    let val () = if !Flags.verbosity >= 1
                then TextIO.print (PP.pp_decl env (A.Exec(f,ext)) ^ "\n")
                else ()
        val p = init_pot env f
        val config = Exec.exec env [A.Proc(0,(0,p),A.ExpName(f,[]))]
        (* may raise Exec.SoftError/Exec.HardError *)
        val () = if !Flags.verbosity >= 1
                then TextIO.print (PP.pp_config (measure (!Flags.time))
                (measure (!Flags.work)) config
                                    ^ "%------------------------------\n")
                else ()
    in
        run env decls
    end
  | run env (_::decls) = run env decls
  | run env nil = ()
*)


let rast_file =
  C.Command.Arg_type.create
    (fun filename ->
      match C.Sys.is_file filename with
          `No | `Unknown ->
            begin
              C.eprintf "'%s' is not a regular file.\n%!" filename;
              exit 1
            end
        | `Yes ->
            if Filename.check_suffix filename ".rast"
            then filename
            else
              begin
                C.eprintf "'%s' does not have rast extension.\n%!" filename;
                exit 1
              end);;

let command =
  C.Command.basic
    ~summary:"Typechecking Rast files"
    ~readme:(fun () -> "More detailed information")
    C.Command.Let_syntax.(
      let%map_open
        verbosity_flag = flag "-v" (optional int)
          ~doc:"verbosity 0: quiet, 1: default, 2: verbose, 3: debugging mode"
        and work_flag = flag "-w" (optional string)
          ~doc:"work-cost-model none, recv, send, recvsend, free"
        and syntax_flag = flag "-s" (optional string)
          ~doc:"syntax implicit, explicit"
        and file = anon("filename" %: rast_file) in
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
          let () = List.iter process_option [vlevel; work_cm; syntax] in
          try
            let _env = load file in print_string ("% file processing successful!\n")
          with ErrorMsg.Error -> C.eprintf "%% file processing failed!\n"; exit 1);;