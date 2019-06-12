module R = Arith
module A = Ast
val cost_recv :
  (A.expression -> A.expression) -> A.expression -> A.expression
val cost_recv_branches :
  (A.expression -> A.expression) -> A.branches -> A.branches
val cost_send :
  (A.expression -> A.expression) -> A.expression -> A.expression
val cost_send_branches :
  (A.expression -> A.expression) -> A.branches -> A.branches
val cost_model :
  (A.expression -> A.expression) ->
  Flags.cost -> A.expression -> A.expression
val apply_cost_work : A.expression -> A.expression
