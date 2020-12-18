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
    parens prec_left 2 (pp_arith_prec 1 s ^ "*" ^ pp_arith_prec 2 t)
  | R.Var(v) -> v;;

(* pp_arith e = "e" *)
let pp_arith e = pp_arith_prec 0 e;;

(*******************************)
(* Types, and their components *)
(*******************************)

(* pp_pot p = "{p}", "" if p = 0 *)
let pp_pot e = match e with
    A.Arith (R.Int 0) -> ""
  | A.Star -> "{*}"
  | A.Arith e -> "{" ^ R.pp_arith e ^ "}";;

(* pp_pospos p = "{p}", "" if p = 1 *)
let pp_potpos e = match e with
    A.Arith (R.Int 1) -> ""
  | A.Star -> "{*}"
  | A.Arith e -> "{" ^ R.pp_arith e ^ "}";;

let pp_arith_opr opr = match opr with
    A.Add -> " + "
  | A.Sub -> " - "
  | A.Mult -> " * "
  | A.Div -> " / ";;

let pp_comp_opr opr = match opr with
    A.Eq -> " = "
  | A.Neq -> " <> "
  | A.Lt -> " < "
  | A.Gt -> " > "
  | A.Leq -> " <= "
  | A.Geq -> " >= ";;

let pp_rel_opr opr = match opr with
    A.And -> "&&"
  | A.Or -> "||";;

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

let pp_mode m = match m with
    A.Unknown -> "U"
  | A.Shared -> "S"
  | A.Transaction -> "T"
  | A.Linear -> "L"
  | A.Pure -> "P"
  | A.MVar v -> v;;

let rec pp_ftp_simple t = match t with
    A.Integer -> "int"
  | A.Boolean -> "bool"
  | A.String -> "string"
  | A.Address -> "address"
  | A.ListTP(t,pot) -> pp_ftp_simple t ^ " list " ^ pp_pot pot
  | A.Arrow(t1,t2) -> pp_ftp_simple t1 ^ " -> " ^ pp_ftp_simple t2
  | A.VarT s -> s;;

let rec pp_tp_simple a = match a with
    A.One -> "1"
  | A.Plus(choice) -> "+{ " ^ pp_choice_simple choice ^ " }"
  | A.With(choice) -> "&{ " ^ pp_choice_simple choice ^ " }"
  | A.Tensor(a,b,m) -> pp_tp_simple a ^ " *[" ^ pp_mode m ^ "] " ^ pp_tp_simple b
  | A.Lolli(a,b,m) -> pp_tp_simple a ^ " -o[" ^ pp_mode m ^ "] " ^ pp_tp_simple b
  | A.GetPot(pot,a) -> "<" ^ pp_potpos pot ^ "| " ^ pp_tp_simple a
  | A.PayPot(pot,a) -> "|" ^ pp_potpos pot ^ "> " ^ pp_tp_simple a
  | A.Up(a) -> "/\\ " ^ pp_tp_simple a
  | A.Down(a) -> "\\/ " ^ pp_tp_simple a
  | A.FArrow(t,a) -> pp_ftp_simple t ^ " -> " ^ pp_tp_simple a
  | A.FProduct(t,a) -> pp_ftp_simple t ^ " ^ " ^ pp_tp_simple a 
  | A.TpName(a) -> a
  | A.FMap(kt,vt) -> "Map<" ^ pp_ftp_simple kt ^ ", " ^ pp_ftp_simple vt ^ ">"
  | A.STMap(kt,vt) -> "Map<" ^ pp_ftp_simple kt ^ ", " ^ pp_tp_simple vt ^ ">"
  | A.Coin -> "coin"

and pp_choice_simple cs = match cs with
    [] -> ""
  | [(l,a)] -> l ^ " : " ^ pp_tp_simple a
  | (l,a)::cs' ->
      l ^ " : " ^ pp_tp_simple a ^ ", " ^ pp_choice_simple cs';;

exception ImpossMode

let pp_outer_mode m = match m with
    A.Unknown -> raise ImpossMode
  | A.Shared -> "contract"
  | A.Transaction -> "transaction"
  | A.Linear -> raise ImpossMode
  | A.Pure -> "asset"
  | A.MVar _v -> raise ImpossMode;;

let pp_structure s = match s with
    A.Hash -> "#"
  | A.Dollar -> "$";;

let pp_chan (s,c,m) = pp_structure s ^ c ^ "[" ^ pp_mode m ^ "]";;

let pp_chan_tp (c,a) = "(" ^ pp_chan c ^ " : " ^ pp_tp_simple a ^ ")";;

let rec pp_channames chans = match chans with
    [] -> ""
  | [c] -> pp_chan c
  | c::chans' -> pp_chan c ^ " " ^ pp_channames chans';;

(* pp_tp i A = "A", where i is the indentation after a newline
 * A must be externalized, or internal name '%n' will be printed
 *)
let rec pp_tp i a = match a with
    A.Plus(choice) -> "+{ " ^ pp_choice (i+3) choice ^ " }"
  | A.With(choice) -> "&{ " ^ pp_choice (i+3) choice ^ " }"
  | A.Tensor(a,b,m) ->
      let astr = pp_tp i a in
      let inc = len astr in
      let s = " *[" ^ pp_mode m ^ "] " in
      let l = len s in
      astr ^ s ^ pp_tp (i+inc+l) b
  | A.Lolli(a,b,m) ->
      let astr = pp_tp i a in
      let inc = len astr in
      let s = " -o[" ^ pp_mode m ^ "] " in
      let l = len s in
      astr ^ s ^ pp_tp (i+inc+l) b
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
  | A.FArrow(t,a) ->
      let tstr = pp_ftp_simple t in
      let inc = len tstr in
      let s = " -> " in
      let l = len s in
      tstr ^ s ^ pp_tp (i+inc+l) a
  | A.FProduct(t,a) ->
      let tstr = pp_ftp_simple t in
      let inc = len tstr in
      let s = " ^ " in
      let l = len s in
      tstr ^ s ^ pp_tp (i+inc+l) a
  | A.TpName(v) -> v
  | A.FMap(kt,vt) ->
      let mapstr = "Map<" in
      let kstr = pp_ftp_simple kt in
      let mapkstr = mapstr ^ kstr ^ ", " in
      mapkstr ^ pp_ftp_simple vt ^ ">"
  | A.STMap(kt,vt) ->
      let mapstr = "Map<" in
      let kstr = pp_ftp_simple kt in
      let mapkstr = mapstr ^ kstr ^ ", " in
      let inc = len mapkstr in
      mapkstr ^ pp_tp (i+inc) vt ^ ">"
  | A.Coin -> "coin"

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

let rec pp_args args = 
  match args with
    | A.Single(x, _) -> x
    | A.Curry((x, _), rest) ->
        let a = pp_args rest in
        x ^ " " ^ a;;

let rec pp_lsctx env delta = match delta with
    [] -> "."
  | [(x,a)] -> "(" ^ pp_chan x ^ " : " ^ pp_tp_compact env a ^ ")"
  | (x,a)::delta' -> "(" ^ pp_chan x ^ " : " ^ pp_tp_compact env a ^ ")" ^ ", " ^ pp_lsctx env delta';;

let pp_arg env xa = match xa with
    A.Functional(v,t) -> "(" ^ v ^ " : " ^ pp_ftp_simple t ^ ")"
  | A.STyped(x,a) -> "(" ^ pp_chan x ^ " : " ^ pp_tp_compact env a ^ ")"

let rec pp_arglist env ctx = match ctx with
    [] -> "."
  | [xa] -> pp_arg env xa
  | xa::ctx' -> pp_arg env xa ^ ", " ^ pp_arglist env ctx';;

let pp_ctx env delta = pp_arglist env delta.A.ordered;;

(* pp_tp_compact env delta pot a = "delta |{p}- C", on one line *)
let pp_tpj_compact env delta pot (x,a) =
  pp_ctx env delta ^ " |" ^ pp_pot pot ^ "- (" ^
  pp_chan x ^ " : " ^ pp_tp_compact env a ^ ")";;

let pp_printable x = 
  match x with
      A.Word(s) -> s 
    | A.PInt ->  "%d"
    | A.PBool -> "%b"
    | A.PStr ->  "%s"
    | A.PAddr -> "%a"
    | A.PChan -> "%c"
    | A.PNewline -> "\\n";;

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
    A.Fwd(x,y) -> pp_chan x ^ " <- " ^ pp_chan y
  | A.Spawn(x,f,xs,q) -> (* exp = x <- f <- xs ; q *)
      pp_chan x ^ " <- " ^ f ^ " " ^ pp_argnames env xs ^ " ;\n"
      ^ pp_exp_indent env i q
  | A.ExpName(x,f,xs) -> pp_chan x ^ " <- " ^ f ^ " " ^ pp_argnames env xs
  | A.Lab(x,k,p) -> pp_chan x ^ "." ^ k ^ " ;\n" ^ pp_exp_indent env i p
  | A.Case(x,bs) -> "case " ^ pp_chan x ^ " ( " ^ pp_branches env (i+8+len (pp_chan x)) bs ^ " )"
  | A.Send(x,w,p) -> "send " ^ pp_chan x ^ " " ^ pp_chan w ^ " ;\n" ^ pp_exp_indent env i p
  | A.Recv(x,y,p) -> pp_chan y ^ " <- recv " ^ pp_chan x ^ " ;\n" ^ pp_exp_indent env i p
  | A.Close(x) -> "close " ^ pp_chan x
  | A.Wait(x,q) -> "wait " ^ pp_chan x ^ " ;\n" ^ pp_exp_indent env i q
  | A.Work(pot, p) ->
      let potstr = pp_potpos pot in
      "work " ^ potstr ^ ";\n" ^ pp_exp_indent env i p
  | A.Deposit(pot, p) ->
      let potstr = pp_potpos pot in
      "Nomos.deposit " ^ potstr ^ ";\n" ^ pp_exp_indent env i p
  | A.Pay(x,pot,p) ->
      let potstr = pp_potpos pot in
      "pay " ^ pp_chan x ^ " " ^ potstr ^ ";\n" ^ pp_exp_indent env i p
  | A.Get(x,pot,q) ->
      let potstr = pp_potpos pot in
      "get " ^ pp_chan x ^ " " ^ potstr ^ ";\n" ^ pp_exp_indent env i q
  | A.Acquire(x,y,p) -> pp_chan y ^ " <- acquire " ^ pp_chan x ^ " ;\n" ^ pp_exp_indent env i p
  | A.Accept(x,y,p) -> pp_chan y ^ " <- accept " ^ pp_chan x ^ " ;\n" ^ pp_exp_indent env i p
  | A.Release(x,y,p) -> pp_chan y ^ " <- release " ^ pp_chan x ^ " ;\n" ^ pp_exp_indent env i p
  | A.Detach(x,y,p) -> pp_chan y ^ " <- detach " ^ pp_chan x ^ " ;\n" ^ pp_exp_indent env i p
  | A.RecvF(x,v,p) -> v ^ " = recv " ^ pp_chan x ^ " ;\n" ^ pp_exp_indent env i p
  | A.SendF(x,e,p) -> "send " ^ pp_chan x ^ " (" ^ pp_fexp env i e.A.func_structure ^ ") ;\n" ^ pp_exp_indent env i p
  | A.Let(v,e,p) -> "let " ^ v ^ " = " ^ pp_fexp env i e.A.func_structure ^ " ;\n" ^ pp_exp_indent env i p
  | A.IfS(e,p1,p2) -> "if " ^ pp_fexp env i e.A.func_structure ^ "\n" ^ pp_then i ^ pp_exp_indent env (i+2) p1 ^ "\n" ^ pp_else i ^ pp_exp_indent env (i+2) p2
  | A.FMapCreate(mp,kt,vt,p) -> pp_chan mp ^ " <- new Map<" ^ pp_ftp_simple kt ^ ", " ^ pp_ftp_simple vt ^ "> ;\n" ^ pp_exp_indent env i p
  | A.STMapCreate(mp,kt,vt,p) -> pp_chan mp ^ " <- new Map<" ^ pp_ftp_simple kt ^ ", " ^ pp_tp_simple vt ^ "> ;\n" ^ pp_exp_indent env i p
  | A.FMapInsert(mp,k,v,p) -> pp_chan mp ^ ".insert(" ^ pp_fexp env i k.A.func_structure ^ ", " ^ pp_fexp env i v.A.func_structure ^ ") ;\n" ^ pp_exp_indent env i p
  | A.STMapInsert(mp,k,v,p) -> pp_chan mp ^ ".insert(" ^ pp_fexp env i k.A.func_structure ^ ", " ^ pp_chan v ^ ") ;\n" ^ pp_exp_indent env i p
  | A.FMapDelete(v,mp,k,p) -> v ^ " = " ^ pp_chan mp ^ ".delete(" ^ pp_fexp env i k.A.func_structure ^ ") ;\n" ^ pp_exp_indent env i p
  | A.STMapDelete(v,mp,k,p) -> pp_chan v ^ " <- " ^ pp_chan mp ^ ".delete(" ^ pp_fexp env i k.A.func_structure ^ ") ;\n" ^ pp_exp_indent env i p
  | A.MapClose(mp,p) -> pp_chan mp ^ ".close ;\n" ^ pp_exp_indent env i p
  | A.Abort -> "abort"
  | A.Print(l,args,p) -> "print(" ^ pp_printable_list env l args ^ ");\n" ^ pp_exp_indent env i p


and pp_printable_list env l args = 
  let s1 = List.map pp_printable l in
  let s1'  = List.fold_left (fun x y -> x ^ y) "\"" s1 in
  let s2 = pp_argnames env args in
  match args with
      [] -> s1' ^ "\""
    | _ -> s1' ^ "\", " ^ s2

and pp_exp_indent env i p = spaces i ^ pp_exp env i p.A.st_structure
and pp_exp_after env i s p = s ^ pp_exp env (i+len(s)) p

and pp_then i = spaces i ^ "then\n"
and pp_else i = spaces i ^ "else\n"

and pp_branches env i bs = match bs with
    [] -> ""
  | [(l,p)] ->
      pp_exp_after env i (l ^ " => ") p.A.st_structure
  | (l,p)::bs' ->
      pp_exp_after env i (l ^ " => ") p.A.st_structure ^ "\n"
      ^ pp_branches_indent env i bs'

and pp_branches_indent env i bs = spaces (i-2) ^ "| " ^ pp_branches env i bs

and pp_fexp_list env i l =
  match l with
      [] -> ""
    | x::xs ->
        let a = pp_fexp env i x.A.func_structure in
        let b = pp_fexp_list env i xs in
        if List.length xs = 0
        then a
        else a ^ "; " ^ b

and pp_fexp env i e = 
  match e with
    | A.If(e1, e2, e3) ->
        let s1 = pp_fexp env i e1.A.func_structure in
        let s2 = pp_fexp_indent env (i+4) e2.A.func_structure in
        let s3 = pp_fexp env (i+4) e3.A.func_structure in
        "if " ^ s1 ^ "\n" ^ pp_then i ^ s2 ^ "\n" ^ pp_else i ^ s3
    | A.Var(x)  -> x
    | A.Bool(b) -> string_of_bool b
    | A.Int(i)  -> string_of_int i
    | A.Str(s) -> s
    | A.Addr(s) -> s
    | A.LetIn(x, e1, e2) ->
        let a = pp_fexp env i e1.A.func_structure in
        let b = pp_fexp_indent env i e2.A.func_structure in
        "let " ^ x ^ " = " ^ a ^ " in \n" ^ b
    | A.ListE(l) ->
        let a = pp_fexp_list env i l in
        "[" ^ a ^ "]"
    | A.Op(e1, opr, e2) ->
        let a = pp_fexp env i e1.A.func_structure in
        let b = pp_fexp env i e2.A.func_structure in
        a ^ pp_arith_opr opr ^ b
    | A.CompOp(e1, opr, e2) ->
        let a = pp_fexp env i e1.A.func_structure in
        let b = pp_fexp env i e2.A.func_structure in
        a ^ pp_comp_opr opr ^ b
    | A.EqAddr(e1, e2) ->
        let a = pp_fexp env i e1.A.func_structure in
        let b = pp_fexp env i e2.A.func_structure in
        a ^ " == " ^ b
    | A.RelOp(e1, opr, e2) ->
        let a = pp_fexp env i e1.A.func_structure in
        let b = pp_fexp env i e2.A.func_structure in
        a ^ pp_rel_opr opr ^ b
    | A.Cons(head, tail) ->
        let a = pp_fexp env i head.A.func_structure in
        let b = pp_fexp env i tail.A.func_structure in
        a ^ "::" ^ b
    | A.Match(x,y,a,b,z) ->
        let sx = pp_fexp env i x.A.func_structure in
        let snil = "| [] -> " in
        let lnil = len snil in
        let sy = pp_fexp env (i+2+lnil) y.A.func_structure in
        let scons = a ^ "::" ^ b ^ " -> " in
        let lcons = len scons in
        let sz = pp_fexp env (i+2+lcons) z.A.func_structure in
        "match " ^ sx ^ " with \n" ^
        spaces (i+2) ^ snil ^ sy ^ "\n" ^
        spaces (i+2) ^ scons ^ sz
    | A.Lambda(args, body) ->
        let p = pp_args args in
        let sargs = "fun " ^ p ^ " -> " in
        let largs = len sargs in
        let q = pp_fexp env (i+largs) body.A.func_structure in
        sargs ^ q
    | A.App l ->
        pp_fexp_list env i l
    | A.Tick(pot,e) -> "(tick " ^ pp_potpos pot ^ " ; " ^ pp_fexp env i e.A.func_structure ^ ")"
    | A.MapSize(mp) -> pp_chan mp ^ ".size"
    | A.GetTxnNum -> "Nomos.GetTxnNum()"
    | A.GetTxnSender -> "Nomos.GetTxnSender()"
    | A.Command(exp) -> "{\n" ^ pp_exp_indent env (i+2) exp ^ "\n" ^ spaces i ^ "}"

and pp_fexp_indent env i p = spaces i ^ pp_fexp env i p

and pp_argname env arg = match arg with
    A.FArg(v) -> pp_fexp env 0 v
  | A.STArg(c) -> pp_chan c

and pp_argnames env args = match args with
    [] -> ""
  | [a] -> pp_argname env a
  | a::args' -> pp_argname env a ^ " " ^ pp_argnames env args';;

let pp_exp_prefix exp = match exp with
    A.Fwd(x,y) -> pp_chan x ^ " <- " ^ pp_chan y
  | A.Spawn(x,f,xs,_q) -> (* exp = x <- f <- xs ; q *)
      pp_chan x ^ " <- " ^ f ^ " <- " ^ pp_argnames () xs ^ " ; ..."
  | A.ExpName(x,f,xs) -> pp_chan x ^ " <- " ^ f ^ " <- " ^ pp_argnames () xs
  | A.Lab(x,k,_p) -> pp_chan x ^ "." ^ k ^ " ; ..."
  | A.Case(x,_bs) -> "case " ^ pp_chan x ^ " ( ... )"
  | A.Send(x,w,_p) -> "send " ^ pp_chan x ^ " " ^ pp_chan w ^ " ; ..."
  | A.Recv(x,y,_p) -> pp_chan y ^ " <- recv " ^ pp_chan x ^ " ; ..."
  | A.Close(x) -> "close " ^ pp_chan x
  | A.Wait(x,_q) -> "wait " ^ pp_chan x ^ " ; ..."
  | A.Work(pot, _p) ->
      let potstr = pp_potpos pot in 
      "work " ^ potstr ^ "; ..."
  | A.Deposit(pot, _p) ->
      let potstr = pp_potpos pot in 
      "deposit " ^ potstr ^ "; ..."
  | A.Pay(x,pot,_p) ->
      let potstr = pp_potpos pot in
      "pay " ^ pp_chan x ^ " " ^ potstr ^ "; ..."
  | A.Get(x,pot,_q) ->
      let potstr = pp_potpos pot in
      "get " ^ pp_chan x ^ " " ^ potstr ^ "; ..."
  | A.Acquire(x,y,_p) -> pp_chan y ^ " <- acquire " ^ pp_chan x ^ " ; ..."
  | A.Accept(x,y,_p) -> pp_chan y ^ " <- accept " ^ pp_chan x ^ " ; ..."
  | A.Release(x,y,_p) -> pp_chan y ^ " <- release " ^ pp_chan x ^ " ; ..."
  | A.Detach(x,y,_p) -> pp_chan y ^ " <- detach " ^ pp_chan x ^ " ; ..."
  | A.RecvF(x,v,_p) -> v ^ " = recv " ^ pp_chan x ^ " ; ..."
  | A.SendF(x,e,_p) -> "send " ^ pp_chan x ^ " (" ^ pp_fexp () 0 e.A.func_structure ^ ") ; ..."
  | A.Let(v,e,_p) -> "let " ^ v ^ " = " ^ pp_fexp () 0 e.A.func_structure ^ " ; ..."
  | A.IfS(e,_p1,_p2) -> "if " ^ pp_fexp () 0 e.A.func_structure ^ " ... "
  | A.FMapCreate(mp,kt,vt,_p) -> pp_chan mp ^ " <- new Map<" ^ pp_ftp_simple kt ^ ", " ^ pp_ftp_simple vt ^ "> ; ..."
  | A.STMapCreate(mp,kt,vt,_p) -> pp_chan mp ^ " <- new Map<" ^ pp_ftp_simple kt ^ ", " ^ pp_tp_simple vt ^ "> ; ..."
  | A.FMapInsert(mp,k,v,_p) -> pp_chan mp ^ ".insert(" ^ pp_fexp () 0 k.A.func_structure ^ ", " ^ pp_fexp () 0 v.A.func_structure ^ ") ; ..."
  | A.STMapInsert(mp,k,v,_p) -> pp_chan mp ^ ".insert(" ^ pp_fexp () 0 k.A.func_structure ^ ", " ^ pp_chan v ^ ") ; ..."
  | A.FMapDelete(v,mp,k,_p) -> v ^ " = " ^ pp_chan mp ^ ".delete(" ^ pp_fexp () 0 k.A.func_structure ^ ") ; ..."
  | A.STMapDelete(v,mp,k,_p) -> pp_chan v ^ " <- " ^ pp_chan mp ^ ".delete(" ^ pp_fexp () 0 k.A.func_structure ^ ") ; ..."
  | A.MapClose(mp,_p) -> pp_chan mp ^ ".close ; ..."
  | A.Abort -> "abort"
  | A.Print(l,args,_) -> "print(" ^ pp_printable_list () l args ^ "); ..."

let rec pp_val_list l =
  match l with
    [] -> ""
  | x::xs ->
      let a = pp_val x  in
      let b = pp_val_list xs in
      if List.length xs = 0 
      then a
      else a ^ "; " ^ b
and pp_val v =
  match v with 
      A.IntV(v1) -> string_of_int v1 
    | A.BoolV(v1) -> string_of_bool v1
    | A.StrV(s) -> s
    | A.AddrV(a) -> a
    | A.ListV(l) -> "[" ^ pp_val_list l ^ "]"
    | A.LambdaV(args, v1) -> "fun " ^ pp_args args ^ " -> " ^ pp_fexp () 0 v1.A.func_structure;;

let pp_msg m = match m with
    A.MLabI(c,k,c') -> "+ " ^ pp_chan c ^ "." ^ k ^ " ; " ^ pp_exp_prefix (Fwd(c,c'))
  | A.MLabE(c,k,c') -> "- " ^ pp_chan c ^ "." ^ k ^ " ; " ^ pp_exp_prefix (Fwd(c',c))
  | A.MSendT(c,e,c') -> "+ " ^ "send " ^ pp_chan c ^ " " ^ pp_chan e ^ " ; " ^ pp_exp_prefix (Fwd(c,c'))
  | A.MSendL(c,e,c') -> "- " ^ "send " ^ pp_chan c ^ " " ^ pp_chan e ^ " ; " ^ pp_exp_prefix (Fwd(c',c))
  | A.MClose(c) -> "close " ^ pp_chan c
  | A.MPayP(c,pot,c') ->
      let potstr = pp_potpos pot in
      "+ " ^ "pay " ^ pp_chan c ^ " " ^ potstr ^ " ; " ^ pp_exp_prefix (Fwd(c,c'))
  | A.MPayG(c,pot,c') ->
      let potstr = pp_potpos pot in
      "- " ^ "pay " ^ pp_chan c ^ " " ^ potstr ^ " ; " ^ pp_exp_prefix (Fwd(c',c))
  | A.MSendP(c,e,c') -> "+ " ^ "send " ^ pp_chan c ^ " " ^ pp_val e ^ " ; " ^ pp_exp_prefix (Fwd(c,c'))
  | A.MSendA(c,e,c') -> "- " ^ "send " ^ pp_chan c ^ " " ^ pp_val e ^ " ; " ^ pp_exp_prefix (Fwd(c',c));;

(****************)
(* Declarations *)
(****************)

let pp_decl env dcl = match dcl with
    A.TpDef(v,a) ->
      pp_tp_after 0 ("type " ^ v ^ " = ") a
  | A.ExpDecDef(f,m,(delta,pot,(x,a)),p) ->
    let potstr = pp_pot pot in
    "proc " ^ pp_outer_mode m ^ " " ^ f ^ " : " ^ pp_ctx env delta ^ " |" ^ potstr ^ "- "
    ^ pp_chan_tp (x,a) ^ " = \n" ^
    (pp_fexp_indent env 0 p.A.func_structure)
  | A.Exec(f, l) -> "exec " ^ f ^ " " ^ pp_argnames env l;;

let pp_progh env decls = List.fold_left (fun s (d, _ext) -> s ^ pp_decl env d ^ "\n") "" decls;;

let pp_prog env = pp_progh env env;;