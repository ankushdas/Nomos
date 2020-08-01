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
              true, _ -> C.eprintf "%% txn sender must be specified (flag ts) to create account\n"; exit 1
            | _, Some _ -> C.eprintf "%% txn sender must be specified (flag ts) to deposit gas\n"; exit 1
            | false, None -> ()
        end
    | Some _ -> ();;

let check_file_validity create deposit file =
  match create, deposit with
      false, None ->
        begin
          match file with
              None -> C.eprintf "%% file must be specified\n"; exit 1
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

let maybe_create_account create config_in config_out =
  if create
  then
    let initial_config = maybe_load_config config_in in
    let final_config = TL.create_account initial_config in
    let () = if !F.verbosity >= 0 then print_string ("% account creation of " ^ !E.txnSender ^ " successful!\n") in

    (* save final configuration *)
    match config_out with
        None -> ()
      | Some(path) -> TL.save_config final_config path;;

let maybe_deposit deposit config_in config_out =
  match deposit with
      None -> ()
    | Some d ->
        let initial_config = maybe_load_config config_in in
        let final_config = TL.deposit_gas d initial_config in
        let () = if !F.verbosity >= 0 then print_string ("% deposit of " ^ string_of_int d ^ " gas units to account of " ^ !E.txnSender ^ " successful!\n") in

        (* save final configuration *)
        match config_out with
            None -> ()
          | Some(path) -> TL.save_config final_config path;;

let maybe_tc_and_run_txn tc_only file config_in config_out =
  match file with
      None -> ()
    | Some file ->
        (* parse *)
        let trans = TL.read file in
        let () = if !F.verbosity >= 0 then print_string ("% parsing successful!\n") in
        (* typecheck *)
        let env = try TL.infer trans
                  with ErrorMsg.Error -> C.eprintf "%% compilation failed!\n"; exit 1
        in
        let () = if !F.verbosity >= 0 then print_string ("% compilation successful!\n") in

        if tc_only
        then
          ()
        else
          (* run transaction *)
          let initial_config = maybe_load_config config_in in
          let final_config = TL.run env initial_config in
          let () = if !F.verbosity >= 0 then print_string ("% runtime successful!\n") in

          (* save final configuration *)
          match config_out with
              None -> ()
            | Some(path) -> TL.save_config final_config path;;

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
        and config_in = flag "-i" (optional in_conf_file)
          ~doc:"input configuration file path"
        and config_out = flag "-o" (optional out_conf_file)
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
          let () = check_validity_cmdline_options run_only tc_only config_in config_out txn_sender create deposit file in
          (* set flags *)
          let () = set_flags verbosity cost_model syntax randomness txn_sender run_only in
          let () = maybe_create_account create config_in config_out in
          let () = maybe_deposit deposit config_in config_out in
          let () = maybe_tc_and_run_txn tc_only file config_in config_out in
          ());;