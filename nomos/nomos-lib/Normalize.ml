(* Normalizing Arithmetic Expressions *)
(*
  N ::= 0 | S
  P ::= A * V | A * 1
  V ::= v | v * V
  A ::= n
  S = P | P + S
*)

module R = Arith

type normal = R.arith;;

exception NormImpossible;;
 
(* R.Int can be applied to negative numbers here *)
let check_coeff e = match e with
    R.Float(_n) -> true
  | R.Var _
  | R.Add _
  | R.Sub _
  | R.Mult _ -> false;;

let check_var_or_one e = match e with
    R.Float(n) ->
      if R.fequals n 1.
      then true
      else false
  | R.Var _ -> true
  | R.Add _
  | R.Sub _
  | R.Mult _ -> false;;


let check_prod e = match e with
    R.Mult(e1,e2) -> check_coeff e1 && check_var_or_one e2
  | R.Var _
  | R.Float _
  | R.Add _
  | R.Sub _ -> false;;

let rec check_sum e = match e with
    R.Add(e1,e2) -> check_prod e1 && check_sum e2
  | R.Float _
  | R.Var _
  | R.Sub _
  | R.Mult _ -> false;;

let check_normal e = match e with
    R.Float(n) ->
      if R.fequals n 0.
      then true
      else check_sum e
  | e -> check_sum e;;

let rec add s1 s2 = match s1 with
    R.Add(p1,s1) -> R.Add(p1, add s1 s2)
  | R.Float(n) ->
      if R.fequals n 0.
      then s2
      else R.Add(s1,s2)
  | p1 -> R.Add(p1,s2);;

let pminus e = match e with
    R.Mult(R.Float(n),p) -> R.Mult(R.Float(-.n), p)
  | _e -> raise NormImpossible;;

let rec sminus e = match e with
    R.Add(p,s) -> R.Add(pminus p, sminus s)
  | R.Float(n) ->
      if R.fequals n 0.
      then R.Float(0.)
      else pminus e
  | p -> pminus p;;

let subtract s1 s2 = add s1 (sminus s2);;

let rec ppmultiply p1 p2 = match p1 with
    R.Mult(a1,p1) -> R.Mult(a1, ppmultiply p1 p2)
  | R.Float(n) ->
      if R.fequals n 0.
      then R.Float(0.)
      else R.Mult(p1,p2)
  | a -> R.Mult(a,p2);;

let rec pmultiply p1 s2 = match s2 with
    R.Add(p2,s2) -> R.Add(ppmultiply p1 p2, pmultiply p1 s2)
  | R.Float(n) ->
      if R.fequals n 0.
      then R.Float(0.)
      else ppmultiply p1 s2
  | p2 -> ppmultiply p1 p2;;

let rec smultiply s1 s2 = match s1 with
    R.Add(p1,s1) -> add (pmultiply p1 s2) (smultiply s1 s2)
  | R.Float(n) ->
      if R.fequals n 0.
      then R.Float(0.)
      else pmultiply s1 s2
  | p1 -> pmultiply p1 s2;;

let rec insert v l = match l with
    [] -> [v]
  | v'::vs' -> if (v <= v') then v::v'::vs'
               else v'::(insert v vs');;

let mult_left a p = match a with
    R.Float(n) ->
      if R.fequals n 0.
      then R.Float(0.)
      else R.Mult(a, p)
  | a -> R.Mult(a, p);;

let add_right e1 e2 = match e2 with
    R.Float(n) ->
      if R.fequals n 0.
      then e1
      else R.Add(e1, e2)
  | e2 -> R.Add(e1, e2);;

let add_left e1 e2 = match e1 with
    R.Float(n) ->
      if R.fequals n 0.
      then e2
      else R.Add(e1,e2)
  | e1 -> R.Add(e1, e2);;

let rec coeff c vs e = match e with 
    R.Mult(R.Float(n),p) -> coeff (c *. n) vs p
  | R.Mult(R.Var(v),p) -> coeff c (insert v vs) p
  | R.Float(n) -> (c *. n,vs)
  | R.Var(v) -> (c, insert v vs)
  | _t -> raise NormImpossible;;

let rec create_term l = match l with
    [] -> R.Float(1.)
  | [v] -> R.Var(v)
  | v::vs -> R.Mult(R.Var(v), create_term vs);;

let add_coeff e1 e2 = match e1, e2 with
    R.Float(n1), R.Float(n2) -> R.Float(n1+.n2)
  | _e1, _e2 -> raise NormImpossible;;

let rec addin (a1,p1) e = match e with
    R.Add(R.Mult(a2,p2),s2) ->
      if (p1 = p2) then add_left (mult_left (add_coeff a1 a2) p1) s2
      else add_right (R.Mult(a2,p2)) (addin (a1,p1) s2)
  | R.Mult(a2,p2) ->
      if (p1 = p2) then mult_left (add_coeff a1 a2) p1
      else add_right (R.Mult(a2,p2)) (mult_left a1 p1)
  | R.Float(n) ->
      if R.fequals n 0.
      then R.Mult(a1,p1)
      else raise NormImpossible
  | _t -> raise NormImpossible;;

let rec sreduce e = match e with
    R.Float(n) ->
      if R.fequals n 0.
      then R.Float(0.)
      else sreduce_h e
  | e -> sreduce_h e

and sreduce_h e = match e with
  | R.Add(p,s) ->
      let s' = sreduce s in
      let (c,vs) = coeff 1. [] p in
      let a = R.Float(c) in
      let p' = create_term vs in
      if R.fequals c 0. then s' else addin (a,p') s'
  | p ->
      let (c,vs) = coeff 1. [] p in
      let a = R.Float(c) in
      let p' = create_term vs in
      mult_left a p';;

let areduce e = match e with
    R.Float(n) -> R.Mult(R.Float(n), R.Float(1.))
  | R.Var(v) -> R.Mult(R.Float(1.), R.Var(v))
  | _t -> raise NormImpossible;;

let rec normalize e = match e with
    R.Float(n) -> areduce (R.Float(n))
  | R.Add(e1,e2) -> sreduce (add (normalize e1) (normalize e2))
  | R.Sub(e1,e2) -> sreduce (subtract (normalize e1) (normalize e2))
  | R.Mult(e1,e2) -> sreduce (smultiply (normalize e1) (normalize e2))
  | R.Var(v) -> areduce (R.Var(v))
  
(* structure Normalize *)
 