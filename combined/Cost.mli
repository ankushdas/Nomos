module R = Arith
module A = Ast
val cost_recv :
  ('a A.st_expr -> 'a A.st_expr) -> 'a A.st_expr -> 'a A.st_expr
val cost_recv_aug :
  ('a A.st_expr -> 'a A.st_expr) -> 'a A.st_aug_expr -> 'a A.st_aug_expr
val cost_recv_branches :
  ('a A.st_expr -> 'a A.st_expr) -> 'a A.branches -> 'a A.branches
val cost_tick_aug : 'a A.func_aug_expr -> 'a A.func_aug_expr
val cost_tick : 'a A.func_expr -> 'a A.func_expr
val cost_model :
  ('a A.st_expr -> 'a A.st_expr) ->
  Flags.cost -> 'a A.st_expr -> 'a A.st_expr
val apply_cost_work : 'a A.st_aug_expr -> 'a A.st_aug_expr
val cost_send :
  ('a A.st_expr -> 'a A.st_expr) -> 'a A.st_expr -> 'a A.st_expr
val cost_send_aug :
  ('a A.st_expr -> 'a A.st_expr) -> 'a A.st_aug_expr -> 'a A.st_aug_expr
val cost_send_branches :
  ('a A.st_expr -> 'a A.st_expr) -> 'a A.branches -> 'a A.branches
val apply_cost : 'a A.func_aug_expr -> 'a A.func_aug_expr
