module A = Ast
module R = Arith

val remove_stars_tp : A.stype -> A.stype
val remove_stars_ftp : A.func_tp -> A.func_tp
val remove_stars_faug : 'a A.func_aug_expr -> 'a A.func_aug_expr
val remove_star : A.potential -> A.potential
val substitute_tp : A.stype -> (string * int) list -> (string * A.mode) list -> A.stype
val substitute_ftp : A.func_tp -> (string * int) list -> (string * A.mode) list -> A.func_tp
val substitute_pot : A.potential -> (string * int) list -> A.potential
val substitute_mode : A.chan -> (string * A.mode) list -> A.chan
val substitute_faug : 'a A.func_aug_expr -> (string * int) list -> (string * A.mode) list -> 'a A.func_aug_expr
val removeU : A.chan -> A.chan
val removeU_tp : A.stype -> A.stype
val removeU_faug : 'a A.func_aug_expr -> 'a A.func_aug_expr
val eq : R.arith -> R.arith -> bool
val ge : R.arith -> R.arith -> bool
val solve_and_print : unit -> (string * int) list * (string * A.mode) list
val reset : unit -> unit
val m_eq : string -> string -> bool
val m_eq_const : string -> A.mode -> bool
val m_lin : string -> bool
val m_eq_pair : string -> A.mode -> A.mode -> bool
val print_stats : unit -> unit