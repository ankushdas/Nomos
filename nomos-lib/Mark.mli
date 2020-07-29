type ext = (int * int) * (int * int) * string [@@deriving sexp]
val show : (int * int) * (int * int) * string -> string
val show_source : (int * int) * (int * int) * string -> string
type 'a marked = 'a * ext option
