module A = Ast
module R = Arith

val remove_stars_tp : A.stype -> A.stype
val remove_stars_exp : A.expression -> A.expression
val remove_star : A.potential -> A.potential
val substitute_tp : A.stype -> (string * int) list -> A.stype
val substitute_exp : A.expression -> (string * int) list -> A.expression
val substitute : A.potential -> (string * int) list -> A.potential
val eq : R.arith -> R.arith -> bool
val ge : R.arith -> R.arith -> bool
val solve_and_print : unit -> (string * int) list
val reset : unit -> unit