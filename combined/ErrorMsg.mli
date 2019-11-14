val anyErrors : bool ref
val reset : unit -> unit
type error_cat = Lex | Parse | Type | Pragma | Runtime
val err_string : error_cat -> string
val tabToSpace : string -> string
val omap : ('a -> 'b) -> 'a option -> 'b option
val pmsg : string -> string -> unit
val error_msg : error_cat -> string -> unit
val warn : error_cat -> string -> unit
exception Error
val error : error_cat -> string -> 'a
