module A = Ast

let (prog_with_cost : A.program ref) = ref [];;

let add_cost_exp d =
  prog_with_cost := d::(!prog_with_cost);;