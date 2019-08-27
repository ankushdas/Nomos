(* Interface for RastConfig *)

module A = Ast
module C = Core

val rast_file : string C.Command.Arg_type.t
val load : string -> A.decl_ext list
val rast_command : C.Command.t