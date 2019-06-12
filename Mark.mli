type ext = (int * int) * (int * int) * string
val pos : int * int -> string
val show : (int * int) * (int * int) * string -> string
val show' : ((int * int) * (int * int) * string) option -> string
val theLine : string -> string
val inputLines : int -> in_channel -> string option
val createLine : int -> int -> string
val count_whitespace : int -> string -> int
val show_source : (int * int) * (int * int) * string -> string
type 'a marked = 'a * ext option
val mark : 'a * 'b -> 'a * 'b option
val mark' : 'a * 'b -> 'a * 'b
val data : 'a * 'b -> 'a
val ext : 'a * 'b -> 'b
val extmin : ('a * 'b) * ('a * 'b) -> 'a * 'b
val extmax : ('a * 'b) * ('a * 'b) -> 'a * 'b
val wrap :
  (('a * 'b) * ('c * 'd) * 'e) option list ->
  (('a * 'b) * ('c * 'd) * 'e) option
val map : ('a -> 'b) -> 'a * 'c -> 'b * 'c
val map' : ('a * 'b -> 'c) -> 'a * 'b -> 'c * 'b
