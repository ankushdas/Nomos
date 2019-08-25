module T = Terminal
module M = TokStream
type lexresult = T.terminal * (int * int);;
val makeLexer : string -> unit -> (T.terminal * (int * int)) M.front
