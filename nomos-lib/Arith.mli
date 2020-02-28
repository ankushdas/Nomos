(* Interface for Arith *)

type arith =
    Int of int
  | Add of arith * arith
  | Sub of arith * arith
  | Mult of arith * arith
  | Var of string
[@@deriving sexp]
val evaluate : arith -> int
val plus : arith -> arith -> arith
val minus : arith -> arith -> arith
val pos : arith -> bool
val non_neg : arith -> bool
val pp_arith : arith -> string
val pp_uneq : arith -> arith -> string
val pp_lt : arith -> arith -> string
exception NotClosed
