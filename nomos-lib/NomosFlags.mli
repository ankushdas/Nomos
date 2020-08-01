(* Interface for Flags *)

type cost = Nil | Free | Recv | RecvSend | Send
val parseCost : string -> cost option
val pp_cost : cost -> string
type syntax = Implicit | Explicit
val parseSyntax : string -> syntax option
val pp_syntax : syntax -> string
val parseRand : string -> bool
val pp_rand : bool -> string
val work : cost ref
val syntax : syntax ref
val verbosity : int ref
val random : bool ref
val bc_mode : bool ref
val reset : unit -> unit