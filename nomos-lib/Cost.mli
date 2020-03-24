module R = Arith
module A = Ast
module F = NomosFlags
val cost_tick_aug : 'a A.func_aug_expr -> 'a A.func_aug_expr
val cost_tick : 'a A.func_expr -> 'a A.func_expr
val cost_work : F.cost -> 'a A.st_aug_expr -> 'a A.st_aug_expr
val work : 'a -> 'a A.st_expr -> 'a A.st_expr
val cost_recv : 'a -> 'a A.st_expr -> 'a A.st_expr
val cost_recv_aug : 'a A.st_aug_expr -> 'a A.st_aug_expr
val cost_recv_branches : 'a A.branches -> 'a A.branches
val cost_send : 'a -> 'a A.st_expr -> 'a A.st_expr
val cost_send_aug : 'a A.st_aug_expr -> 'a A.st_aug_expr
val cost_send_branches : 'a A.branches -> 'a A.branches
val apply_cost : 'a A.func_aug_expr -> 'a A.func_aug_expr
