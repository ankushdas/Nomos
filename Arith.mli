type arith =
    Int of int
  | Add of arith * arith
  | Sub of arith * arith
  | Mult of arith * arith
val evaluate : arith -> int
val plus : arith -> arith -> arith
val minus : arith -> arith -> arith
val pos : arith -> bool
val non_neg : arith -> bool
val eq : arith -> arith -> bool
val ge : arith -> arith -> bool
val pp_arith : arith -> string
val pp_uneq : arith -> arith -> string
val pp_lt : arith -> arith -> string
