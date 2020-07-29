open Sexplib.Std
type arith =
    Int of int            (* ..., -1, 0, 1, ... *)
  | Add of arith * arith  (* e1 + e2 *)
  | Sub of arith * arith  (* e1 - e2 *)
  | Mult of arith * arith (* e1 * e2 *)
  | Var of string
[@@deriving sexp]

exception NotClosed

(* evaluate e = k if e |->* k *)
(* assumes . |- e int, raises NotClosed otherwise *)
let rec evaluate e = match e with
  | Int(k) -> k
  | Add(e1,e2) -> (evaluate e1) + (evaluate e2)
  | Sub(e1,e2) -> (evaluate e1) - (evaluate e2)
  | Mult(e1,e2) -> (evaluate e1) * (evaluate e2)
  | Var(_v) -> raise NotClosed;;

let plus e1 e2 = Add(e1,e2);;

let minus e1 e2 = Sub(e1,e2);;

let non_neg e = evaluate e >= 0;;

let rec pp_arith e = match e with
  | Int(n) -> string_of_int n
  | Add(s,t) -> "(" ^ pp_arith s ^ "+" ^ pp_arith t ^ ")"
  | Sub(s,t) -> "(" ^ pp_arith s ^ "-" ^ pp_arith t ^ ")"
  | Mult(s,t) -> "(" ^ pp_arith s ^ "*" ^ pp_arith t ^ ")"
  | Var(v) -> v;;

let pp_uneq e1 e2 = pp_arith e1 ^ " != " ^ pp_arith e2;;

let pp_lt e1 e2 = pp_arith e1 ^ " < " ^ pp_arith e2;;
