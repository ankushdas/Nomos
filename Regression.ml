(* Simple regression testing for rast *)

module A = Ast
module F = Flags
module R = Rast
module C = Core

exception OS_FAILURE;;
exception OS_SUCCESS;;
exception RegressionImpossible;;

type outcome =
    StaticError            (* includes lexer, parser, type-checker error *)
  | Success                (* parses, type-checks, and runs successfully *)
  (* remaining ones are never expected, right now *)
  | DynamicError           (* should not happen *)
  | UncaughtException      (* uncaught exception should always be a bug *)
  | IllegalTestFormat      (* #test pragma not parsable *)
  | FileNotReadable        (* file is not readable *)
  | None;;                 (* no outcome at all... *)

let parse_outcome str = match str with
    ["error"] -> StaticError
  | ["success"] -> Success
  | _ -> IllegalTestFormat;;

let pp_outcome oc = match oc with
    StaticError -> "static error (lexing, parsing, type-checking)"
  | Success -> "success"
  | DynamicError -> "dynamic error (execution)"
  | UncaughtException -> "uncaught exception"
  | IllegalTestFormat -> "illegal #test format"
  | FileNotReadable -> "file not readable"
  | None -> "none";;

let rec extract_outcome pragmas = 
  match pragmas with
  (* nothing specified, default to Success *)
    [] -> Success
  | {A.declaration = A.Pragma("#test", line); A.decl_extent = _ext}::_preamble ->
    (* ignore remaining preamble *)
    parse_outcome (String.split_on_char ' ' line)
  | {A.declaration = A.Pragma _; A.decl_extent = _ext}::preamble ->
    extract_outcome preamble
    (* should only be pragmas allowed here *)
  | _ -> raise RegressionImpossible;;

exception Outcome of outcome * outcome;; (* expected, actual *)

let load_file expected filename =
  let _env = try R.load filename
            with ErrorMsg.Error -> raise (Outcome (expected, StaticError))
  in
  try raise (Outcome(expected, Success))
  with
    (Outcome (_expected, _actual) as e) -> raise e
  | _e -> raise (Outcome (expected, UncaughtException));;

let run_file filename =
  let () = Parsestate.reset () in
  let () = ErrorMsg.reset () in
  let () = Flags.reset () in
  let () = Flags.verbosity := -1 in (* really quiet *)
  let preamble = try Parse.parse_preamble filename
                 with Sys_error _e -> raise (Outcome (None, FileNotReadable)) in
  let expected = try extract_outcome preamble
                 with ErrorMsg.Error -> raise (Outcome (None, IllegalTestFormat)) in
  try load_file expected filename
  with 
    (Outcome(_expected, _actual) as e) -> raise e
  | _e -> raise (Outcome (None, UncaughtException));;

let total = ref 0;;
let succeeded = ref 0;;
let failed = ref 0;;

let success (_expected, _actual) =
    print_string "[OK]\n"
  ; succeeded := !succeeded+1;;

let failure (expected, actual) =
    print_string ("[FAIL]\n")
  ; print_string ("Expected: " ^ pp_outcome expected ^ "\n")
  ; print_string ("Actual:   " ^ pp_outcome actual ^ "\n")
  ; failed := !failed+1;;

let test_file filename =
    print_string (filename ^ "... ")
  ; flush stdout
  ; try run_file filename
    with Outcome(expected, actual) ->
        total := !total+1
      ; if expected = actual
        then success (expected, actual)
        else failure (expected, actual);;

let reset_counts () =
    total := 0
  ; succeeded := 0
  ; failed := 0;;

let print_results () =
    print_string ("Total tests: " ^ string_of_int (!total) ^ "\n")
  ; print_string ("Succeeded:   " ^ string_of_int (!succeeded) ^ "\n")
  ; print_string ("Failed:      " ^ string_of_int (!failed) ^ "\n");;

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
    ~summary:"Regression Testing"
    ~readme:(fun () -> "More detailed information")
    C.Command.Let_syntax.(
      let%map_open
        verbosity_flag = flag "-v" (optional int)
          ~doc:"verbosity 0: quiet, 1: default, 2: verbose, 3: debugging mode"
        and files = anon (sequence ("filename" %: rast_file)) in
        fun () ->
          let () = reset_counts () in
          let () =
            begin
              match verbosity_flag with
                  None -> F.verbosity := 0
                | Some n -> F.verbosity := n
            end
          in
          let () = List.iter test_file files in
          let () = print_results () in
          if !total = !succeeded
          then print_string ("regression testing successful!\n")
          else C.eprintf "regression testing failed!\n"; exit 1);;

let () =
  C.Command.run ~version:"1.0" ~build_info:"RWO" command;;
