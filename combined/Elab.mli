module R = Arith
module A = Ast
module PP = Pprint
module TC = Typecheck
module E = TpError
module I = Infer
val error : string -> 'a
val postponed : A.decl -> string
val pp_costs : unit -> string
val dups : ('a * 'b) list -> bool
val check_nonneg : A.potential -> 'a -> unit
val commit : A.decl list -> A.argument list -> A.argument list -> A.context
val elab_tps : TC.A.decl list -> A.decl list -> 'a -> A.decl list
exception ElabImpossible
val elab_exps' : A.decl list -> A.decl list -> 'a -> A.decl list
val elab_exps : A.decl list -> A.decl list -> 'a -> A.decl list
val elab_decls : TC.A.decl list -> A.decl list -> 'a -> A.decl list option
val is_tpdef : A.decl list -> A.tpname -> bool
val is_expdecdef : A.decl list -> A.expname -> bool
val check_redecl : A.decl list -> A.decl list -> unit
val commit_channels : A.decl list -> A.decl list -> A.decl list
val remove_stars_tps : A.decl list -> A.decl list
val remove_stars_exps : A.decl list -> A.decl list
val remove_stars : A.decl list -> A.decl list
val removeU_tps : A.decl list -> A.decl list
val removeU_exps : A.decl list -> A.decl list
val removeU : A.decl list -> A.decl list
val well_formedness :
  'a ->
  string ->
  A.mode -> TC.A.context -> TC.PP.A.str * string * TC.PP.A.mode -> 'b -> unit
val gen_constraints : 'a -> A.decl list -> 'b -> unit
val substitute :
  A.decl list ->
  (string * int) list -> (string * I.A.mode) list -> A.decl list
