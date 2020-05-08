module A = Ast
module E = Exec

type environment = (A.decl * A.ext) list

type raw_transaction = RawTransaction of environment
type transaction = Transaction of environment

(* flags like cost model, etc should be set by modifying the relevant globals *)

(* holds a full configuration so we don't have to thread it through our commands *)
val gconfig : E.full_configuration ref

(* resets gconfig to an empty configuration *)
val reset : unit -> unit

(* loads a file to gconfig *)
val load : string -> unit

(* saves gconfig to a file *)
val save : string -> unit

(* read an environment from a file *)
val read : string -> raw_transaction

(* typecheck and eliminate stars *)
val infer : raw_transaction -> transaction

(* execute all the execs in an environment *)
val exec : transaction -> unit

(* directly execute a file in an environment *)
val load_and_exec : string -> unit

(* show the shared channels and their types *)
(*val show_channels : unit*)

(* used for the command line *)
val run : transaction -> E.full_configuration -> E.full_configuration

val load_config : string -> E.full_configuration

val save_config : E.full_configuration -> string -> unit
