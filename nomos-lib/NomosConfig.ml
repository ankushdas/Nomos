(* Nomos command-line tool entry point *)

open TopLevel
module R = Arith
module A = Ast
module PP = Pprint
module C = Core
module EL = Elab
module I = Infer
module TC = Typecheck
module F = NomosFlags
module E = Exec
module Map = C.Map

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

(*********************)
(* Utility Functions *)
(*********************)
let maybe_load_config config_in_opt =
  match config_in_opt with
  | None -> E.empty_full_configuration
  | Some(path) -> load_config path


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
          let contract_env = read file in
          let () = print_string ("% contract parsing successful!\n") in
          let txn_env =
            match txn_path with
                None -> []
              | Some(txn_file) -> 
                  let e = read txn_file in
                  begin
                    print_string "% transaction parsing successful!\n";
                    e
                  end
          in

          (* typecheck *)
          let env = try infer (contract_env @ txn_env)
                    with ErrorMsg.Error -> C.eprintf "%% compilation failed!\n"; exit 1
          in
          let () = print_string ("% compilation successful!\n") in

          if tc_only
            then ()
            else
              (* run transaction *)
              let initial_config = maybe_load_config config_in in
              let final_config = run env initial_config env in
              let () = print_string ("% runtime successful!\n") in

              (* save final configuration *)
              match config_out with
                  None -> ()
                | Some(path) -> save_config final_config path)
