type ext = (int * int) * (int * int) * string
val pos : int * int -> string
val show : (int * int) * (int * int) * string -> string
val theLine : string -> string
val inputLines : int -> in_channel -> string option
val createLine : int -> int -> string
val count_whitespace : int -> string -> int
val show_source : (int * int) * (int * int) * string -> string
type 'a marked = 'a * ext option
val mark' : 'a * 'b -> 'a * 'b
val data : 'a * 'b -> 'a
val ext : 'a * 'b -> 'b
