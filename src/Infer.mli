module A = Ast
module R = Arith

val remove_stars : A.decl_ext list -> A.decl_ext list
val eq : R.arith -> R.arith -> bool
val ge : R.arith -> R.arith -> bool