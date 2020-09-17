(* Nomos command-line tool entry point *)

module TL = TopLevel
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
module EM = ErrorMsg

let error m = EM.error EM.Pragma None m;;
           
(************************)
(* Command Line Options *)
(************************)

let set_cost_model s =
  match F.parseCost s with
      None -> error ("% cost model " ^ s ^ " not recognized\n")
    | Some cm -> F.work := cm;;

let set_syntax s =
  match F.parseSyntax s with
      None -> error ("% syntax " ^ s ^ " not recognized\n")
    | Some syn -> F.syntax := syn

let set_txn_sender s =
  match s with
      None -> ()
    | Some s -> TL.set_sender s;;

let set_bc_mode m =
  if m then F.bc_mode := false;;

let check_extension filename ext =
  if Filename.check_suffix filename ext
  then filename
  else
    EM.error EM.File None ("'" ^ filename ^ "' does not have " ^ ext ^ " extension!\n");;
    
let file (ext : string) =
  C.Command.Arg_type.create
    (fun filename ->
      match C.Sys.is_file filename with
          `No | `Unknown ->
            begin
              EM.error EM.File None ("'" ^ filename ^ "' is not a regular file!\n")
            end
        | `Yes -> check_extension filename ext)

let nomos_file = file ".nom"

let in_conf_file = file ".conf"

let out_conf_file =
  C.Command.Arg_type.create
    (fun filename -> check_extension filename ".conf")

(*********************)
(* Utility Functions *)
(*********************)

let maybe_load_config config_in_opt =
  match config_in_opt with
  | None -> E.empty_blockchain_state
  | Some(path) -> TL.load_config path

let create_or_deposit txn_sender create deposit =
  match txn_sender with
      None ->
        begin
          match create, deposit with
              true, _ ->
                error ("% txn sender must be specified (flag ts) to create account\n")
            | _, Some _ -> error ("%% txn sender must be specified (flag ts) to deposit gas\n")
            | false, None -> ()
        end
    | Some _ -> ();;

let check_file_validity create deposit file =
  match create, deposit with
      false, None ->
        begin
          match file with
              None -> EM.error EM.File None ("% file must be specified\n")
            | Some _ -> ()
        end
    | _, _ -> ();;

let check_validity_cmdline_options run_only _tc_only _config_in _config_out txn_sender create deposit file =
  if not run_only
  then
    let () = create_or_deposit txn_sender create deposit in
    let () = check_file_validity create deposit file in
    ();;

let set_flags verbosity cost_model syntax randomness txn_sender run_only =
  let () = F.reset () in
  let () = F.verbosity := verbosity in
  let () = set_cost_model cost_model in
  let () = set_syntax syntax in
  let () = F.random := F.parseRand randomness in
  let () = set_txn_sender txn_sender in
  let () = set_bc_mode run_only in
  ();;

let maybe_create_account create initial_config =
  if create
  then
    let final_config = TL.create_account !E.txnSender initial_config in
    final_config
  else
    initial_config;;

let maybe_deposit deposit initial_config =
  match deposit with
      None -> initial_config
    | Some d ->
        let final_config = TL.deposit_gas !E.txnSender d initial_config in
        final_config;;

let maybe_tc_and_run_txn tc_only file initial_config =
  try
    match file with
        None -> initial_config
      | Some file ->
          (* parse *)
          let rawtxn = TL.read file in
          let () = if !F.verbosity >= 0 then print_string ("% parsing successful!\n") in
          (* typecheck *)
          let env = TL.infer initial_config rawtxn in
          let () = if !F.verbosity >= 0 then print_string ("% compilation successful!\n") in

          if tc_only
          then
            initial_config
          else
            (* run transaction *)
            let (final_config, _leftover_gas) = TL.run env initial_config in
            let () = if !F.verbosity >= 0 then print_string ("% runtime successful!\n") in
            final_config
            
  with
    | EM.LexError msg
    | EM.ParseError msg
    | EM.TypeError msg
    | EM.PragmaError msg
    | EM.RuntimeError msg
    | EM.GasAcctError msg
    | EM.FileError msg -> print_string (msg ^ "\n"); exit 1;;

let maybe_save_config final_config config_out_file =
    (* save final configuration *)
    match config_out_file with
        None -> ()
      | Some(path) -> TL.save_config final_config path;;


(****************)
(* Main Command *)
(****************)

let nomos_command =
  C.Command.basic
    ~summary:"Typechecking and Executing Nomos files"
    C.Command.Let_syntax.(
      let%map_open
        verbosity = flag "-v" (optional_with_default 0 int)
          ~doc:"verbosity:- 0: quiet, 1: default, 2: verbose, 3: debugging mode"
        and cost_model = flag "-w" (optional_with_default "none" string)
          ~doc:"work-cost-model: none, recv, send, recvsend, free"
        and syntax = flag "-s" (optional_with_default "explicit" string)
          ~doc:"syntax: implicit, explicit"
        and tc_only = flag "-tc" no_arg
          ~doc:"type check only"
        and config_in_file = flag "-i" (optional in_conf_file)
          ~doc:"input configuration file path"
        and config_out_file = flag "-o" (optional out_conf_file)
          ~doc:"output configuration file path"
        and txn_sender = flag "-ts" (optional string)
          ~doc:"transaction sender's address"
        and randomness = flag "-rand" (optional_with_default "yes" string)
          ~doc:"random semantics: no, yes"
        and create = flag "-create" no_arg
          ~doc:"create gas account"
        and deposit = flag "-deposit" (optional int)
          ~doc:"deposit gas into sender's account"
        and run_only = flag "-run" no_arg
          ~doc:"check and run non-blockchain program"
        and file = anon(maybe ("filename" %: nomos_file)) in
        fun () ->
          (* check flag consistency *)
          let () = check_validity_cmdline_options run_only tc_only config_in_file config_out_file txn_sender create deposit file in
          (* set flags *)
          let () = set_flags verbosity cost_model syntax randomness txn_sender run_only in
          let config = maybe_load_config config_in_file in
          let config = maybe_create_account create config in
          let config = maybe_deposit deposit config in
          let config = maybe_tc_and_run_txn tc_only file config in
          let () = maybe_save_config config config_out_file in
          ());;
