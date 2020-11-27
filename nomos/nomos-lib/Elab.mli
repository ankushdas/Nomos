module R = Arith
module A = Ast
module PP = Pprint
module TC = Typecheck
module E = TpError
module I = Infer
module F = NomosFlags
val error : ((int * int) * (int * int) * string) option -> string -> 'a
val postponed : A.decl -> string
val pp_costs : unit -> string
val dups : ('a * 'b) list -> bool
val check_nonneg :
  A.potential -> ((int * int) * (int * int) * string) option -> unit
val elab_tps :
  (TC.A.decl * 'a) list ->
  (A.decl * ((int * int) * (int * int) * string) option) list ->
  (A.decl * ((int * int) * (int * int) * string) option) list
exception ElabImpossible
val elab_exps' :
  (A.decl * 'a) list -> (A.decl * TC.A.ext) list -> (A.decl * TC.A.ext) list
val elab_exps :
  (A.decl * 'a) list -> (A.decl * TC.A.ext) list -> (A.decl * TC.A.ext) list
val elab_decls :
  (TC.A.decl * 'a) list ->
  (A.decl * TC.A.ext) list -> (A.decl * TC.A.ext) list option
val is_tpdef : (A.decl * 'a) list -> A.tpname -> bool
val is_expdecdef : (A.decl * 'a) list -> A.expname -> bool
val check_redecl :
  (A.decl * ((int * int) * (int * int) * string) option) list -> unit
val check_declared :
  (A.decl * 'a) list ->
  ((int * int) * (int * int) * string) option -> A.stype -> unit
val check_declared_pchoices :
  (A.decl * 'a) list ->
  ((int * int) * (int * int) * string) option -> A.pchoices -> unit
val check_declared_choices :
  (A.decl * 'a) list ->
  ((int * int) * (int * int) * string) option -> A.choices -> unit
val check_declared_proc :
  (A.decl * 'a) list ->
  ((int * int) * (int * int) * string) option A.st_aug_expr -> unit
val check_declared_branches :
  (A.decl * 'a) list ->
  ((int * int) * (int * int) * string) option A.branches -> unit
val check_declared_prog :
  (A.decl * 'a) list ->
  ((int * int) * (int * int) * string) option A.func_aug_expr -> unit
val check_declared_list :
  (A.decl * 'a) list ->
  ((int * int) * (int * int) * string) option -> A.argument list -> unit
val check_declared_ctx :
  (A.decl * 'a) list ->
  ((int * int) * (int * int) * string) option -> A.context -> unit
val check_valid :
  (A.decl * 'a) list ->
  (A.decl * ((int * int) * (int * int) * string) option) list -> unit
val commit :
  (A.decl * 'a) list -> A.argument list -> A.argument list -> A.context
val commit_channels :
  (A.decl * 'a) list -> (A.decl * 'b) list -> (A.decl * 'b) list
val remove_stars_tps : (A.decl * 'a) list -> (A.decl * 'a) list
val remove_stars_exps : (A.decl * 'a) list -> (A.decl * 'a) list
val remove_stars : (A.decl * 'a) list -> (A.decl * 'a) list
val removeU_tps : (A.decl * 'a) list -> (A.decl * 'a) list
val removeU_exps : (A.decl * 'a) list -> (A.decl * 'a) list
val removeU : (A.decl * 'a) list -> (A.decl * 'a) list
val well_formedness :
  'a ->
  string ->
  A.mode ->
  TC.A.context ->
  TC.PP.A.str * string * TC.PP.A.mode ->
  ((int * int) * (int * int) * string) option -> unit
val gen_constraints :
  (TC.A.decl * 'a) list -> (A.decl * TC.A.ext) list -> unit
val substitute :
  (A.decl * 'a) list ->
  (string * float) list -> (string * I.A.mode) list -> (A.decl * 'a) list
