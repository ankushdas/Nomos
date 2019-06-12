module PS = Parsestate
module T = Terminal
module M = TokStream
val error : int * int -> string -> 'a
val isAlpha : char -> bool
val isDigit : char -> bool
val id_start_char : char -> bool
val id_char : char -> bool
val implode : char list -> string
val run_cond :
  (char -> bool) ->
  int * char list * (unit -> char M.front) ->
  string * int * (unit -> char M.front)
exception LexImpossible
val lex_code :
  int * (unit -> Char.t M.front) ->
  T.terminal * int * int * (unit -> Char.t M.front)
val lex_comment_line :
  int * (unit -> Char.t M.front) ->
  T.terminal * int * int * (unit -> Char.t M.front)
val lex_pragma :
  int * (unit -> Char.t M.front) ->
  T.terminal * int * int * (unit -> Char.t M.front)
val lex_comment :
  int * (unit -> char M.front) * int ->
  T.terminal * int * int * (unit -> char M.front)
exception Match
val buffered_stream : string -> unit -> char M.front
type lexresult = T.terminal * (int * int)
val lexer :
  int * (unit -> Char.t M.front) -> (T.terminal * (int * int)) M.front
val makeLexer : string -> unit -> (T.terminal * (int * int)) M.front
