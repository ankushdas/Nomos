(* Cost Model *)
type cost = None                   (* none *)
          | Free                   (* only explicit work or delay constructs *)
          | Recv                   (* each receive costs 1 unit  *)
          | RecvSend               (* each receive and send costs 1 unit *)
          | Send                   (* each send costs 1 unit *)

let parseCost s = match s with
    "none" -> Some None
  | "recv" -> Some Recv
  | "recvsend" -> Some RecvSend
  | "send" -> Some Send
  | "free" -> Some Free
  | _ -> None;;

let pp_cost c = match c with
    None -> "none"
  | Recv -> "recv"
  | RecvSend -> "recvsend"
  | Send -> "send"
  | Free -> "free";;

(* Syntax *)
(* Explicit syntax performs no reconstruction
* Implicit syntax reconstructs non-structural types
* (quantifiers, work, time)
*)
type syntax = Implicit | Explicit

let parseSyntax s = match s with
    "implicit" -> Some Implicit
  | "explicit" -> Some Explicit
  | _ -> None;;

let pp_syntax syn = match syn with
    Implicit -> "implicit"
  | Explicit -> "explicit";;

(* Default values *)
let time = ref None;;
let work = ref None;;
let syntax = ref Implicit;;
let verbosity = ref 1;;           (* -1 = print nothing, 0 = quiet, 1 = normal, 2 = verbose, 3 = debug *)
let help = ref false;;

let reset () =
    time := None
  ; work := None
  ; syntax := Explicit
  ; verbosity := 1
  ; help := false;;