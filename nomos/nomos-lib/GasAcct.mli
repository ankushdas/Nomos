module C = Core
module M = C.Map

type gas_accounts = int M.M(C.String).t [@@deriving sexp]

val create_account : (string, int, 'a) M.t -> string -> (string, int, 'a) M.t

val deposit : (string, int, 'a) M.t -> string -> int -> (string, int, 'a) M.t

type res = Insufficient of int | Balance of int | NonBC;;

val deduct : (string, int, 'a) M.t -> string -> int -> res * (string, int, 'a) M.t