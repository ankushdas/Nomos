(* Interface for SafeIO *)

type 'a result = Value of 'a | Exception of exn
val withOpenIn : string -> (in_channel -> 'a) -> 'a
