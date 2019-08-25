(* Interface for Parsestate *)

val reset : unit -> unit
val pushfile : string -> unit
val popfile : unit -> unit
val newline : int -> unit
val ext : int * int -> ((int * int) * (int * int) * string) option
