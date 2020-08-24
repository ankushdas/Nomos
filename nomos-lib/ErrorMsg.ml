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
    None -> ""
  | Some x -> (f x);;

let pmsg str ext note =
  let filepos = omap Mark.show ext in
  let error_msg = ":" ^ str ^ ":" ^ note ^ "\n" in
  let source = omap (fun x -> tabToSpace (Mark.show_source x)) ext in
  filepos ^ error_msg ^ source;;

let error_msg cat ext note =
  ( anyErrors := true
  ; pmsg (err_string cat ^ " error") ext note);;

(* Print the given error message and then abort compilation *)
exception LexError of string
exception ParseError of string
exception TypeError of string
exception PragmaError of string
exception RuntimeError of string

let error cat ext msg =
  let errormsg = error_msg cat ext msg in
  match cat with
      Lex -> raise (LexError errormsg)
    | Parse -> raise (ParseError errormsg)
    | Type -> raise (TypeError errormsg)
    | Pragma -> raise (PragmaError errormsg)
    | Runtime -> raise (RuntimeError errormsg);;
