module F = NomosFlags

(* Initial values of compiler state variables *)
let anyErrors = ref false;;

let reset () = ( anyErrors := false );;

type error_cat =
    Lex
  | Parse
  | Type
  | Pragma
  | Runtime;;

let err_string cat = match cat with
    Lex -> "lex"
  | Parse -> "parse"
  | Type -> "type"
  | Pragma -> "pragma"
  | Runtime -> "runtime";;

(* We turn tabs into spaces because they are counted as a single character in
    the extents, so in order for the emphasis to be correct we need each
    character to be one column wide. *)
let tabToSpace = String.map (fun c -> match c with | '\t' -> ' ' | c -> c);;

let omap f opt = match opt with
    None -> None
  | Some x -> Some (f x);;

let pmsg str ext note =
  ( ignore (omap (fun x -> print_string (Mark.show x)) ext)
  ; List.iter print_string [":"; str; ":"; note; "\n"]
  ; ignore (omap (fun x -> print_string (tabToSpace (Mark.show_source x))) ext)
  );;

let error_msg cat ext note =
    ( anyErrors := true
    ; if !F.verbosity >= 0 (* verbosity < 0: don't print error messages! *)
      then pmsg (err_string cat ^ " error") ext note
      else () );;

let warn cat ext note = pmsg (err_string cat ^ " warning") ext note;;

(* Print the given error message and then abort compilation *)
exception Error

let error cat ext msg = ( error_msg cat ext msg ; raise Error );;


let pmsg1 str note =
  List.iter print_string [":"; str; ":"; note; "\n"];;

let error_msg1 cat note =
    ( anyErrors := true
    ; if !F.verbosity >= 0 (* verbosity < 0: don't print error messages! *)
      then pmsg1 (err_string cat ^ " error") note
      else () );;

let warn1 cat note = pmsg1 (err_string cat ^ " warning") note;;


let error1 cat msg = ( error_msg1 cat msg ; raise Error );;
