(* Interface for RastConfig *)

module A = Ast
module C = Core

val nomos_file : string C.Command.Arg_type.t
val load : string -> A.decl list
val nomos_command : C.Command.t