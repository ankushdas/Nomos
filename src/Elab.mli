(* Interface for Elab *)

module A = Ast
module TC = Typecheck

val elab_decls :
  TC.A.decl_ext list -> A.decl_ext list -> A.decl_ext list option
val check_redecl : A.decl_ext list -> A.decl_ext list -> unit
val commit_channels : A.decl_ext list -> A.decl_ext list -> A.decl_ext list
val remove_stars : A.decl_ext list -> A.decl_ext list