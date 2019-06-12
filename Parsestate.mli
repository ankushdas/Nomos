val currFilenames : string list ref
val currLiness : int list list ref
val reset : unit -> unit
val pushfile : string -> unit
val popfile : unit -> unit
val newline : int -> unit
val linewidth : unit -> int
val look : int * int list * int -> int * int
val last : unit -> int * int
val ext : int * int -> ((int * int) * (int * int) * string) option
