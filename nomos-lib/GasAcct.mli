module C = Core
module M = C.Map

type gas_accounts = int M.M(C.String).t [@@deriving sexp]

val create_account :
  'a ->
  'b * 'c * ('a, int, 'd) M.t * 'e * 'f ->
  'b * 'c * ('a, int, 'd) M.t * 'e * 'f

val deposit_gas :
  string ->
  int ->
  'a * 'b * (string, int, 'c) M.t * 'd * 'e ->
  'a * 'b * (string, int, 'c) M.t * 'd * 'e

type res = Insufficient of int | Balance of int | RunOnly;;

val deduct : (string, int, 'a) M.t -> string -> int -> res * (string, int, 'a) M.t