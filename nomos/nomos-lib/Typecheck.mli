module A = Ast
val esync_tp :
  (A.decl * 'a) list ->
  A.stype -> ((int * int) * (int * int) * string) option -> unit
val ssync_tp :
  (A.decl * 'a) list ->
  A.stype -> ((int * int) * (int * int) * string) option -> unit
val contractive : A.stype -> bool
val is_tpdef : (A.decl * 'a) list -> string -> bool
val is_expdecdef : (A.decl * 'a) list -> string -> bool
val check_declared : (A.decl * 'a) list -> ((int * int) * (int * int) * string) option -> A.stype -> unit
val check_declared_ctx : (A.decl * 'a) list -> ((int * int) * (int * int) * string) option -> A.context -> unit
val checkfexp :
  bool ->
  (A.decl * 'a) list ->
  A.context ->
  A.potential ->
  A.ext A.func_aug_expr -> A.chan * A.stype -> A.ext -> A.mode -> unit
val pure :
  'a ->
  string ->
  A.context ->
  A.str * string * A.mode ->
  ((int * int) * (int * int) * string) option -> unit
val shared :
  'a ->
  string ->
  A.context ->
  A.str * string * A.mode ->
  ((int * int) * (int * int) * string) option -> unit
val transaction :
  'a ->
  string ->
  A.context ->
  A.str * string * A.mode ->
  ((int * int) * (int * int) * string) option -> unit
