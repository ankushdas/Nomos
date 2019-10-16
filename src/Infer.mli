module A = Ast
module R = Arith

val remove_stars_tp : A.stype -> A.stype
val remove_stars_exp : A.expression -> A.expression
val remove_star : A.potential -> A.potential
val eq : R.arith -> R.arith -> bool
val ge : R.arith -> R.arith -> bool