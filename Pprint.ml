module R = Arith
module A = Ast

(**************************)
(* Arithmetic expressions *)
(**************************)
              
(* Uses precedence
 * prec('+','-') = 1; prec('*') = 2
 *)
let parens prec_left prec s =
    if prec_left >= prec then "(" ^ s ^ ")" else s;;

(* pp_arith_prec prec_left e = "e"
 * using the precedence prec_left of the operator
 * to the left to decide on parentheses
 * All arithmetic operators are left associative
 *)
let rec pp_arith_prec prec_left e = match e with
    R.Int(n) ->
      if n >= 0 then string_of_int n
      else pp_arith_prec prec_left (R.Sub(R.Int(0),R.Int(0-n))) (* might overflow *)
  | R.Add(s,t) ->
      parens prec_left 1 (pp_arith_prec 0 s ^ "+" ^ pp_arith_prec 1 t)
  | R.Sub(s,t) ->
      parens prec_left 1 (pp_arith_prec 0 s ^ "-" ^ pp_arith_prec 1 t)
  | R.Mult(s,t) ->
    parens prec_left 2 (pp_arith_prec 1 s ^ "*" ^ pp_arith_prec 2 t);;

(* pp_arith e = "e" *)
let pp_arith e = pp_arith_prec 0 e;;

(****************)
(* Propositions *)
(****************)

(* omit parenthesis for /\ and \/ and right-associative => *)
(* precedence: ~ > /\,\/,=> *)

type opr = Or | And | Implies | Not | None

let parens_opr opr_above opr s = match opr_above with
    None -> s           (* root: no parentheses *)
  | opr_above ->
      if opr_above = opr then s else "(" ^ s ^ ")";;

(*******************************)
(* Types, and their components *)
(*******************************)

(* pp_pot p = "{p}", "" if p = 0 *)
let pp_pot e = A.pp_pot e;;

(* pp_pospos p = "{p}", "" if p = 1 *)
let pp_potpos e = A.pp_potpos e;;

(***********************)
(* Externalizing types *)
(***********************)

(*******************************)
(* Multiline Printing of Types *)
(*******************************)

let rec spaces n =
    if n <= 0 then ""
    else " " ^ spaces (n-1);;

let len s = String.length s;;

(* pp_tp i A = "A", where i is the indentation after a newline
 * A must be externalized, or internal name '%n' will be printed
 *)
let rec pp_tp i a = match a with
    A.Plus(choice) -> "+{ " ^ pp_choice (i+3) choice ^ " }"
  | A.With(choice) -> "&{ " ^ pp_choice (i+3) choice ^ " }"
  | A.Tensor(a,b) ->
      let astr = pp_tp i a in
      let l = len astr in
      astr ^ " * " ^ pp_tp (i+l+3) b
  | A.Lolli(a,b) ->
      let astr = pp_tp i a in
      let inc = len astr in
      astr ^ " -o " ^ pp_tp (i+inc+4) b
  | A.One -> "1"
  | A.PayPot(pot,a) ->
      let potstr = pp_potpos pot in
      let inc = len potstr in
      "|" ^ potstr ^ "> " ^ pp_tp (i+inc+3) a
  | A.GetPot(pot,a) ->
      let potstr = pp_potpos pot in
      let inc = len potstr in
      "<" ^ potstr ^ "| " ^ pp_tp (i+inc+3) a
  | A.TpName(v) -> v

and pp_tp_indent i a = spaces i ^ pp_tp i a

and pp_tp_after i s a = s ^ pp_tp (i+len(s)) a

and pp_choice i cs = match cs with
    [] -> ""
  | [(l,a)] ->
    pp_tp_after i (l ^ " : ") a
  | (l,a)::cs' ->
    pp_tp_after i (l ^ " : ") a ^ ",\n"
    ^ pp_choice_indent i cs'
and pp_choice_indent i cs = spaces i ^ pp_choice i cs;;

let pp_tp = fun _env -> fun a -> pp_tp 0 a;;

(* pp_tp_compact env A = "A", without newlines
 * this first externalizes A, then prints on one line
 *)
let pp_tp_compact _env a = A.pp_tp a;;

let rec pp_ctx env delta = match delta with
    [] -> "."
  | [(x,a)] -> "(" ^ x ^ " : " ^ pp_tp_compact env a ^ ")"
  | (x,a)::delta' -> "(" ^ x ^ " : " ^ pp_tp_compact env a ^ ")" ^ ", " ^ pp_ctx env delta';;

(* pp_tpj env delta pot (x,a) = "delta |{pot}- (x : a)" *)
let pp_tpj env delta pot (x,a) =
    pp_ctx env delta ^ " |" ^ pp_pot pot ^ "- (" ^
    x ^ " : " ^ pp_tp env a ^ ")";;

(* pp_tp_compact env delta pot a = "delta |{p}- C", on one line *)
let pp_tpj_compact env delta pot (x,a) =
  pp_ctx env delta ^ " |" ^ pp_pot pot ^ "- (" ^
  x ^ " : " ^ pp_tp_compact env a ^ ")";;

(***********************)
(* Process expressions *)
(***********************)

(* Cut is right associative, so we need paren around
 * the left-hand side of a cut if it is not atomic.
 * Atomic are Id, Case<dir>, CloseR, ExpName
 * Rather than propagating a binding strength downward,
 * we just peek ahead.
 *)
let rec atomic p = match p with
    A.Fwd _ | A.Case _ | A.Recv _
  | A.Close _ | A.ExpName _ -> true
  | A.Spawn _
  | A.Lab _ | A.Send _ | A.Wait _
  | A.Work _ | A.Pay _ | A.Get _ -> false
  | A.Marked(marked_exp) -> atomic (Mark.data marked_exp);;

let rec long p = match p with
    A.Case _ -> true
  | A.Marked(marked_exp) -> long (Mark.data marked_exp)
  | A.Fwd _ | A.Spawn _ | A.ExpName _
  | A.Lab _ | A.Send _ | A.Recv _ | A.Close _ | A.Wait _
  | A.Work _ | A.Pay _ | A.Get _ -> false;;

let pp_cut env pot b = match pot with
    R.Int(0) -> "[" ^ pp_tp_compact env b ^ "]"
  | pot -> "[|" ^ pp_pot pot ^ "- " ^ pp_tp_compact env b ^ "]";;

let rec pp_exp env i exp = match exp with
    A.Fwd(x,y) -> x ^ " <- " ^ y
  | A.Spawn(x,f,xs,q) -> (* exp = x <- f <- xs ; q *)
      x ^ " <- " ^ f ^ " <- " ^ A.pp_channames xs ^ " ;\n"
      ^ pp_exp_indent env i q
  | A.ExpName(x,f,xs) -> x ^ " <- " ^ f ^ " <- " ^ A.pp_channames xs
  | A.Lab(x,k,p) -> x ^ "." ^ k ^ " ;\n" ^ pp_exp_indent env i p
  | A.Case(x,bs) -> "case " ^ x ^ " ( " ^ pp_branches env (i+8+len x) bs ^ " )"
  | A.Send(x,w,p) -> "send " ^ x ^ " " ^ w ^ " ;\n" ^ pp_exp_indent env i p
  | A.Recv(x,y,p) -> y ^ " <- recv " ^ x ^ " ;\n" ^ pp_exp_indent env i p
  | A.Close(x) -> "close " ^ x
  | A.Wait(x,q) -> "wait " ^ x ^ " ;\n" ^ pp_exp_indent env i q
  | A.Work(pot, p) -> "work " ^ pp_potpos pot ^ ";\n" ^ pp_exp_indent env i p
  | A.Pay(x,pot,p) -> "pay " ^ x ^ " " ^ pp_potpos pot ^ ";\n" ^ pp_exp_indent env i p
  | A.Get(x,pot,q) -> "get " ^ x ^ " " ^ pp_potpos pot ^ ";\n" ^ pp_exp_indent env i q
  | A.Marked(marked_exp) -> pp_exp env i (Mark.data marked_exp)

and pp_exp_indent env i p = spaces i ^ pp_exp env i p
and pp_exp_after env i s p = s ^ pp_exp env (i+len(s)) p

and pp_branches env i bs = match bs with
    [] -> ""
  | [{lab_exp = (l,p); exp_extent = _ext}] ->
      pp_exp_after env i (l ^ " => ") p
  | {lab_exp = (l,p); exp_extent = _ext}::bs' ->
      pp_exp_after env i (l ^ " => ") p ^ "\n"
      ^ pp_branches_indent env i bs'

and pp_branches_indent env i bs = spaces (i-2) ^ "| " ^ pp_branches env i bs;;

let rec pp_exp_prefix exp = match exp with
    A.Fwd(x,y) -> x ^ " <- " ^ y
  | A.Spawn(x,f,xs,_q) -> (* exp = x <- f <- xs ; q *)
      x ^ " <- " ^ f ^ " <- " ^ A.pp_channames xs ^ " ; ..."
  | A.ExpName(x,f,xs) -> x ^ " <- " ^ f ^ " <- " ^ A.pp_channames xs
  | A.Lab(x,k,_p) -> x ^ "." ^ k ^ " ; ..."
  | A.Case(x,_bs) -> "case " ^ x ^ " ( ... )"
  | A.Send(x,w,_p) -> "send " ^ x ^ " " ^ w ^ " ; ..."
  | A.Recv(x,y,_p) -> y ^ " <- recv " ^ x ^ " ; ..."
  | A.Close(x) -> "close " ^ x
  | A.Wait(x,_q) -> "wait " ^ x ^ " ; ..."
  | A.Work(pot, _p) -> "work " ^ pp_potpos pot ^ "; ..."
  | A.Pay(x,pot,_p) -> "pay " ^ x ^ " " ^ pp_potpos pot ^ "; ..."
  | A.Get(x,pot,_q) -> "get " ^ x ^ " " ^ pp_potpos pot ^ "; ..."
  | A.Marked(marked_exp) -> pp_exp_prefix (Mark.data marked_exp)



(****************)
(* Declarations *)
(****************)

exception Unsupported

let pp_decl env dcl = match dcl with
    A.TpDef(v,a) ->
      pp_tp_after 0 ("type " ^ v ^ " = ") a
  | A.TpEq(A.TpName(v),A.TpName(v')) ->
    "eqtype " ^ v ^ " = " ^ v'
  | A.ExpDecDef(f,(delta,pot,(x,a)),p) ->
    "proc " ^ f ^ " : " ^ pp_ctx env delta ^ " |" ^ pp_pot pot ^ "- "
    ^ A.pp_chan (x,a) ^ " = \n" ^
    (pp_exp_indent env 4 p)
  | A.Exec(f) -> "exec " ^ f
  | A.Pragma(p,line) -> p ^ line
  | A.TpEq(_a,_a') -> raise Unsupported;;

(******************)
(* Configurations *)
(******************)

let pp_config _mtime _mwork conf = A.pp_config conf;;

(**********************)
(* External Interface *)
(**********************)

let pp_exp = fun env -> fun p -> pp_exp env 0 p;;
