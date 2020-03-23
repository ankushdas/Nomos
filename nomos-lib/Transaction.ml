module A = Ast
module C = Core

open Sexplib.Std

type transaction = string * (A.ext A.arg list) [@@deriving sexp]

let load_transaction path = C.Sexp.load_sexp_conv_exn path transaction_of_sexp
