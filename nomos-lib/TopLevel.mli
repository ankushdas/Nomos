module A = Ast
module E = Exec

type environment = (A.decl * A.ext) list [@@deriving sexp]

type raw_transaction = RawTransaction of environment [@@deriving sexp]
type transaction = Transaction of environment [@@deriving sexp]

(* flags like cost model, etc should be set by modifying the relevant globals *)

(* holds a full configuration so we don't have to thread it through our commands *)
val gconfig : E.blockchain_state ref

(* resets gconfig to an empty configuration *)
val reset : unit -> unit

(* loads a file to gconfig *)
val load : string -> unit

(* saves gconfig to a file *)
val save : string -> unit

(* read an environment from a file *)
val read : string -> raw_transaction

(* read a transaction from a string *)
val read_txn : string -> raw_transaction

(* typecheck and eliminate stars *)
val infer : E.blockchain_state -> raw_transaction -> transaction

(* sets the current transaction sender *)
val set_sender : string -> unit

(* create account for a new txn sender *)
val create_account : string -> E.blockchain_state -> E.blockchain_state

(* deposit gas into txn sender's account *)
val deposit_gas : string -> int -> E.blockchain_state -> E.blockchain_state

(* execute all the execs in an environment *)
val exec : transaction -> unit

(* read, typecheck, and execute a file (equiv to exec (infer (read f)))*)
val read_and_exec : string -> unit

(* show the shared channels and their types *)
val show_channels : unit -> unit

(* used for the command line *)
val run : transaction -> E.blockchain_state -> E.blockchain_state * int

val load_config : string -> E.blockchain_state

val save_config : E.blockchain_state -> string -> unit
