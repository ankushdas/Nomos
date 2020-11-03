module A = Ast

val elab_decls :
  (A.decl * 'a) list ->
  (A.decl * A.ext) list -> (A.decl * A.ext) list
val check_redecl :
  (A.decl * ((int * int) * (int * int) * string) option) list -> unit
val check_valid :
  (A.decl * 'a) list ->
  (A.decl * ((int * int) * (int * int) * string) option) list -> unit
val get_one_exec :
  (A.decl * 'a) list -> int -> string -> string
val commit_channels :
  (A.decl * 'a) list -> (A.decl * 'b) list -> (A.decl * 'b) list
val remove_stars : (A.decl * 'a) list -> (A.decl * 'a) list
val removeU : (A.decl * 'a) list -> (A.decl * 'a) list
val gen_constraints :
  (A.decl * 'a) list -> (A.decl * A.ext) list -> unit
val substitute :
  (A.decl * 'a) list ->
  (string * int) list -> (string * A.mode) list -> (A.decl * 'a) list
