module A = Ast

type transaction = string * (A.ext A.arg list) [@@deriving sexp]

val load_transaction : string -> transaction
