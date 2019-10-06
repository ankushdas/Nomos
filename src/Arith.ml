type arith =
  Int of int            (* ..., -1, 0, 1, ... *)
| Add of arith * arith  (* e1 + e2 *)
| Sub of arith * arith  (* e1 - e2 *)
| Mult of arith * arith (* e1 * e2 *)

(* evaluate e = k if e |->* k *)
(* assumes . |- e int, raises NotClosed otherwise *)
let rec evaluate e = match e with
  | Int(k) -> k
  | Add(e1,e2) -> (evaluate e1) + (evaluate e2)
  | Sub(e1,e2) -> (evaluate e1) - (evaluate e2)
  | Mult(e1,e2) -> (evaluate e1) * (evaluate e2);;

let plus e1 e2 = Add(e1,e2);;

let minus e1 e2 = Sub(e1,e2);;

let pos e = evaluate e > 0;;
let non_neg e = evaluate e >= 0;;

let eq e1 e2 = (evaluate e1) = (evaluate e2);;

let ge e1 e2 = (evaluate e1) >= (evaluate e2);;

let rec pp_arith e = match e with
  | Int(n) -> if n >= 0 then string_of_int n else "0-" ^ string_of_int (0-n)
  | Add(s,t) -> "(" ^ pp_arith s ^ "+" ^ pp_arith t ^ ")"
  | Sub(s,t) -> "(" ^ pp_arith s ^ "-" ^ pp_arith t ^ ")"
  | Mult(s,t) -> "(" ^ pp_arith s ^ "*" ^ pp_arith t ^ ")";;

let pp_uneq e1 e2 = pp_arith e1 ^ " != " ^ pp_arith e2;;

let pp_lt e1 e2 = pp_arith e1 ^ " < " ^ pp_arith e2;;