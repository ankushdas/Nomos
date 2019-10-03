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

(*******************************)
(* Types, and their components *)
(*******************************)

(* pp_pot p = "{p}", "" if p = 0 *)
let pp_pot e = match e with
    R.Int(0) -> ""
  | e -> "{" ^ R.pp_arith e ^ "}";;

(* pp_pospos p = "{p}", "" if p = 1 *)
let pp_potpos e = match e with
    R.Int(1) -> ""
  | e -> "{" ^ R.pp_arith e ^ "} ";;

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

let rec pp_tp_simple a = match a with
    A.One -> "1"
  | Plus(choice) -> "+{ " ^ pp_choice_simple choice ^ " }"
  | With(choice) -> "&{ " ^ pp_choice_simple choice ^ " }"
  | Tensor(a,b) -> pp_tp_simple a ^ " * " ^ pp_tp_simple b
  | Lolli(a,b) -> pp_tp_simple a ^ " -o " ^ pp_tp_simple b
  | GetPot(pot,a) -> "<" ^ pp_potpos pot ^ "| " ^ pp_tp_simple a
  | PayPot(pot,a) -> "|" ^ pp_potpos pot ^ "> " ^ pp_tp_simple a
  | Up(a) -> "/\\ " ^ pp_tp_simple a
  | Down(a) -> "\\/ " ^ pp_tp_simple a 
  | TpName(a) -> a

and pp_choice_simple cs = match cs with
    [] -> ""
  | [(l,a)] -> l ^ " : " ^ pp_tp_simple a
  | (l,a)::cs' ->
      l ^ " : " ^ pp_tp_simple a ^ ", " ^ pp_choice_simple cs';;

let rec pp_channames chans = match chans with
    [] -> ""
  | [c] -> c
  | c::chans' -> c ^ " " ^ pp_channames chans';;

let pp_chan (c,a) = "(" ^ c ^ " : " ^ pp_tp_simple a ^ ")";;

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
  | A.Up(a) ->
      "/\\ " ^ pp_tp (i+3) a
  | A.Down(a) ->
      "\\/ " ^ pp_tp (i+3) a
  | A.TpName(v) -> v

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
let pp_tp_compact _env a = pp_tp_simple a;;

let rec pp_lsctx env delta = match delta with
    [] -> "."
  | [(x,a)] -> "(" ^ x ^ " : " ^ pp_tp_compact env a ^ ")"
  | (x,a)::delta' -> "(" ^ x ^ " : " ^ pp_tp_compact env a ^ ")" ^ ", " ^ pp_lsctx env delta';;

let pp_ctx env delta = pp_lsctx env delta.A.shared ^ " ; " ^ pp_lsctx env delta.A.linear;;

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

let rec pp_exp env i exp = match exp with
    A.Fwd(x,y) -> x ^ " <- " ^ y
  | A.Spawn(x,f,xs,q) -> (* exp = x <- f <- xs ; q *)
      x ^ " <- " ^ f ^ " <- " ^ pp_channames xs ^ " ;\n"
      ^ pp_exp_indent env i q
  | A.ExpName(x,f,xs) -> x ^ " <- " ^ f ^ " <- " ^ pp_channames xs
  | A.Lab(x,k,p) -> x ^ "." ^ k ^ " ;\n" ^ pp_exp_indent env i p
  | A.Case(x,bs) -> "case " ^ x ^ " ( " ^ pp_branches env (i+8+len x) bs ^ " )"
  | A.Send(x,w,p) -> "send " ^ x ^ " " ^ w ^ " ;\n" ^ pp_exp_indent env i p
  | A.Recv(x,y,p) -> y ^ " <- recv " ^ x ^ " ;\n" ^ pp_exp_indent env i p
  | A.Close(x) -> "close " ^ x
  | A.Wait(x,q) -> "wait " ^ x ^ " ;\n" ^ pp_exp_indent env i q
  | A.Work(pot, p) -> "work " ^ pp_potpos pot ^ ";\n" ^ pp_exp_indent env i p
  | A.Pay(x,pot,p) -> "pay " ^ x ^ " " ^ pp_potpos pot ^ ";\n" ^ pp_exp_indent env i p
  | A.Get(x,pot,q) -> "get " ^ x ^ " " ^ pp_potpos pot ^ ";\n" ^ pp_exp_indent env i q
  | A.Acquire(x,y,p) -> y ^ " <- acquire " ^ x ^ " ;\n" ^ pp_exp_indent env i p
  | A.Accept(x,y,p) -> y ^ " <- accept " ^ x ^ " ;\n" ^ pp_exp_indent env i p
  | A.Release(x,y,p) -> y ^ " <- release " ^ x ^ " ;\n" ^ pp_exp_indent env i p
  | A.Detach(x,y,p) -> y ^ " <- detach " ^ x ^ " ;\n" ^ pp_exp_indent env i p
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
      x ^ " <- " ^ f ^ " <- " ^ pp_channames xs ^ " ; ..."
  | A.ExpName(x,f,xs) -> x ^ " <- " ^ f ^ " <- " ^ pp_channames xs
  | A.Lab(x,k,_p) -> x ^ "." ^ k ^ " ; ..."
  | A.Case(x,_bs) -> "case " ^ x ^ " ( ... )"
  | A.Send(x,w,_p) -> "send " ^ x ^ " " ^ w ^ " ; ..."
  | A.Recv(x,y,_p) -> y ^ " <- recv " ^ x ^ " ; ..."
  | A.Close(x) -> "close " ^ x
  | A.Wait(x,_q) -> "wait " ^ x ^ " ; ..."
  | A.Work(pot, _p) -> "work " ^ pp_potpos pot ^ "; ..."
  | A.Pay(x,pot,_p) -> "pay " ^ x ^ " " ^ pp_potpos pot ^ "; ..."
  | A.Get(x,pot,_q) -> "get " ^ x ^ " " ^ pp_potpos pot ^ "; ..."
  | A.Acquire(x,y,_p) -> y ^ " <- acquire " ^ x ^ " ; ..."
  | A.Accept(x,y,_p) -> y ^ " <- accept " ^ x ^ " ; ..."
  | A.Release(x,y,_p) -> y ^ " <- release " ^ x ^ " ; ..."
  | A.Detach(x,y,_p) -> y ^ " <- detach " ^ x ^ " ; ..."
  | A.Marked(marked_exp) -> pp_exp_prefix (Mark.data marked_exp);;

let pp_msg m = match m with
    A.MLabI(c,k,c') -> "+ " ^ c ^ "." ^ k ^ " ; " ^ pp_exp_prefix (Fwd(c,c'))
  | A.MLabE(c,k,c') -> "- " ^ c ^ "." ^ k ^ " ; " ^ pp_exp_prefix (Fwd(c',c))
  | A.MSendT(c,e,c') -> "+ " ^ "send " ^ c ^ " " ^ e ^ " ; " ^ pp_exp_prefix (Fwd(c,c'))
  | A.MSendL(c,e,c') -> "- " ^ "send " ^ c ^ " " ^ e ^ " ; " ^ pp_exp_prefix (Fwd(c',c))
  | A.MClose(c) -> "close " ^ c
  | A.MPayP(c,pot,c') -> "+ " ^ "pay " ^ c ^ " " ^ pp_potpos pot ^ " ; " ^ pp_exp_prefix (Fwd(c,c'))
  | A.MPayG(c,pot,c') -> "- " ^ "pay " ^ c ^ " " ^ pp_potpos pot ^ " ; " ^ pp_exp_prefix (Fwd(c',c));;


(*
let rec pp_exp_simple p = match p with
    A.Fwd(x,y) -> x ^ " <- " ^ y
  | Spawn(x,f,xs,q) -> x ^ " <- " ^ f ^ " <- " ^ pp_channames xs ^ " ; " ^ pp_exp_simple q
  | ExpName(x,f,xs) -> x ^ " <- " ^ f ^ " <- " ^ pp_channames xs
  | Lab(x,k,p) -> x ^ "." ^ k ^ " ; " ^ pp_exp_simple p
  | Case (x,bs) -> "case " ^ x ^ " (" ^ pp_branches_simple bs ^ ")"
  | Send(x,w,p) -> "send " ^ x ^ " " ^ w ^ " ; " ^ pp_exp_simple p
  | Recv(x,y,p) -> y ^ " <- recv " ^ x ^ " ; " ^ pp_exp_simple p
  | Close(x) -> "close " ^ x
  | Wait(x,p) -> "wait " ^ x ^ " ; " ^ pp_exp_simple p
  | Work(pot,p) -> "work " ^ pp_potpos pot ^ " ; " ^ pp_exp_simple p
  | Pay(x,pot,p) -> "pay " ^ x ^ " " ^ pp_potpos pot ^ " ; " ^ pp_exp_simple p
  | Get(x,pot,p) -> "get " ^ x ^ " " ^ pp_potpos pot ^ " ; " ^ pp_exp_simple p
  | Acquire(x,y,p) -> y ^ " <- acquire " ^ x ^ " ; " ^ pp_exp_simple p
  | Accept(x,y,p) -> y ^ " <- accept " ^ x ^ " ; " ^ pp_exp_simple p
  | Release(x,y,p) -> y ^ " <- release " ^ x ^ " ; " ^ pp_exp_simple p
  | Detach(x,y,p) -> y ^ " <- detach " ^ x ^ " ; " ^ pp_exp_simple p
  | Marked(marked_p) -> pp_exp_simple (Mark.data marked_p)

and pp_branches_simple bs = match bs with
    [] -> ""
  | [{A.lab_exp = (l,p); exp_extent = _ext}] -> l ^ " => " ^ pp_exp_simple p
  | {A.lab_exp = (l,p); exp_extent = _ext}::bs' ->
    l ^ " => " ^ pp_exp_simple p ^ " | " ^ pp_branches_simple bs';;
*)

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
    ^ pp_chan (x,a) ^ " = \n" ^
    (pp_exp_indent env 4 p)
  | A.Exec(f) -> "exec " ^ f
  | A.Pragma(p,line) -> p ^ line
  | A.TpEq(_a,_a') -> raise Unsupported;;

(**********************)
(* External Interface *)
(**********************)

let pp_exp = fun env -> fun p -> pp_exp env 0 p;;
