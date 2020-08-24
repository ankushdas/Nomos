val reset : unit -> unit
type error_cat = Lex | Parse | Type | Pragma | Runtime
val error_msg :
  error_cat -> ((int * int) * (int * int) * string) option -> string -> string
exception LexError of string
exception ParseError of string
exception TypeError of string
exception PragmaError of string
exception RuntimeError of string
val error :
  error_cat -> ((int * int) * (int * int) * string) option -> string -> 'a
