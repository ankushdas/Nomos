module R = Arith
module A = Ast
module PP = Pprint
module TC = Typecheck
val error : ((int * int) * (int * int) * string) option -> string -> 'a
val postponed : A.decl -> string
val pp_costs : unit -> string
val dups : ('a * 'b) list -> bool
val valid_ctx :
  TC.A.decl_ext list ->
  ('a * TC.A.stype) list ->
  ((int * int) * (int * int) * string) option -> unit
val valid_delta :
  TC.A.decl_ext list ->
  A.context -> ((int * int) * (int * int) * string) option -> unit
val check_nonneg :
  R.arith -> ((int * int) * (int * int) * string) option -> unit
val commit : A.decl_ext list -> (A.chan * A.stype) list -> A.context
val elab_tps : TC.A.decl_ext list -> A.decl_ext list -> A.decl_ext list
exception ElabImpossible
val elab_exps' : A.decl_ext list -> A.decl_ext list -> A.decl_ext list
val elab_exps : A.decl_ext list -> A.decl_ext list -> A.decl_ext list
val elab_decls :
  TC.A.decl_ext list -> A.decl_ext list -> A.decl_ext list option
val is_tpdef : A.decl_ext list -> A.tpname -> bool
val is_expdecdef : A.decl_ext list -> A.expname -> bool
val check_redecl : A.decl_ext list -> A.decl_ext list -> unit
