open Exec
module A = Ast

type environment = (A.decl * A.ext) list

type raw_transaction = RawTransaction of environment
type transaction = Transaction of environment

(* flags like cost model, etc should be set by modifying the relevant globals *)

(* holds a full configuration so we don't have to thread it through our commands *)
val gconfig : full_configuration ref

(* resets gconfig to an empty configuration *)
val reset : unit -> unit

(* loads a file to gconfig *)
val load : string -> unit

(* saves gconfig to a file *)
val save : string -> unit

(* read an environment from a file *)
val read : string -> environment

(* create a raw transaction from a list of environments *)
val build : environment list -> raw_transaction

(* typecheck and eliminate stars *)
val infer : raw_transaction -> transaction

(* execute all the execs in an environment *)
val exec : transaction -> unit

(* directly execute a collection of files in an environment *)
val load_and_exec : string list -> unit

(* show the shared channels and their types *)
(*val show_channels : unit*)

(* used for the command line *)
val run : transaction -> full_configuration -> full_configuration

val load_config : string -> full_configuration

val save_config : full_configuration -> string -> unit
