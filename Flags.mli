type cost = None | Free | Recv | RecvSend | Send
val parseCost : string -> cost option
val pp_cost : cost -> string
type syntax = Implicit | Explicit
val parseSyntax : string -> syntax option
val pp_syntax : syntax -> string
type recursion = Equi | Iso
val parseRecursion : string -> recursion option
val pp_recursion : recursion option -> string
val time : cost ref
val work : cost ref
val syntax : syntax ref
val terminate : recursion option ref
val verbosity : int ref
val help : bool ref
val reset : unit -> unit
