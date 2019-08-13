module A = Ast
module C = Core

val load : string -> A.decl_ext list
val command : C.Command.t