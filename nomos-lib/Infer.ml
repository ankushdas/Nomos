module A = Ast
module R = Arith
module N = Normalize
module S = Solver
module C = Core
module M = C.Map
module PP = Pprint
module F = NomosFlags
module ClpS = S.Clp (S.Clp_std_options);;

exception InferImpossible;;

(************************************************************)
(* Substituting potential variables for star in annotations *)
(************************************************************)
let vnum = ref 0;;

let fresh () =
  let v = "_v" ^ string_of_int !vnum in
  let () = vnum := !vnum + 1 in
  v;;

let remove_star pot = match pot with
    A.Star ->
      let v = fresh () in
      A.Arith(R.Var(v))
  | A.Arith e -> A.Arith e;;

let rec remove_stars_tp tp = match tp with
    A.Plus(choices) -> A.Plus(remove_stars_choices choices)
  | A.With(choices) -> A.With(remove_stars_choices choices)
  | A.Tensor(a,b,m) -> A.Tensor(remove_stars_tp a, remove_stars_tp b,m)
  | A.Lolli(a,b,m) -> A.Lolli(remove_stars_tp a, remove_stars_tp b,m)
  | A.One -> A.One
  | A.PayPot(pot,a) -> A.PayPot(remove_star pot, remove_stars_tp a)
  | A.GetPot(pot,a) -> A.GetPot(remove_star pot, remove_stars_tp a)
  | A.TpName(v) -> A.TpName(v)
  | A.Up(a) -> A.Up(remove_stars_tp a)
  | A.Down(a) -> A.Down(remove_stars_tp a)
  | A.FArrow(t,a) -> A.FArrow(t,remove_stars_tp a)
  | A.FProduct(t,a) -> A.FProduct(t,remove_stars_tp a)

and remove_stars_choices choices = match choices with
    (l,a)::choices' -> (l,remove_stars_tp a)::(remove_stars_choices choices')
  | [] -> []

and remove_stars_ftp ftp = match ftp with
    A.ListTP(t,pot) -> A.ListTP(t, remove_star pot)
  | A.Integer | A.Boolean | A.Address | A.VarT _ -> ftp
  | A.Arrow(t1,t2) -> A.Arrow(remove_stars_ftp t1, remove_stars_ftp t2);;

let rec remove_stars_exp exp = match exp with
    A.Fwd(x,y) -> A.Fwd(x,y)
  | A.Spawn(x,f,xs,q) -> A.Spawn(x,f,xs, remove_stars_aug q)
  | A.ExpName(x,f,xs) -> A.ExpName(x,f,xs)
  | A.Lab(x,k,p) -> A.Lab(x,k, remove_stars_aug p)
  | A.Case(x,branches) -> A.Case(x, remove_stars_branches branches)
  | A.Send(x,w,p) -> A.Send(x,w, remove_stars_aug p)
  | A.Recv(x,y,p) -> A.Recv(x,y, remove_stars_aug p)
  | A.Close(x) -> A.Close(x)
  | A.Wait(x,q) -> A.Wait(x, remove_stars_aug q)
  | A.Work(pot,p) ->
      let pot' = remove_star pot in
      A.Work(pot', remove_stars_aug p)
  | A.Pay(x,pot,p) ->
      let pot' = remove_star pot in
      A.Pay(x, pot', remove_stars_aug p)
  | A.Get(x,pot,p) ->
      let pot' = remove_star pot in
      A.Get(x, pot', remove_stars_aug p)
  | A.Acquire(x,y,p) -> A.Acquire(x,y, remove_stars_aug p)
  | A.Accept(x,y,p) -> A.Accept(x,y, remove_stars_aug p)
  | A.Release(x,y,p) -> A.Release(x,y, remove_stars_aug p)
  | A.Detach(x,y,p) -> A.Detach(x,y, remove_stars_aug p)
  | A.SendF(x,e,p) -> A.SendF(x,e, remove_stars_aug p)
  | A.RecvF(x,y,p) -> A.RecvF(x,y, remove_stars_aug p)
  | A.Let(x,e,p) -> A.Let(x,e, remove_stars_aug p)
  | A.IfS(e,p1,p2) -> A.IfS(e, remove_stars_aug p1, remove_stars_aug p2)
  | A.MakeChan(x,a,n,p) -> A.MakeChan(x, a, n, remove_stars_aug p)

and remove_stars_branches bs = match bs with
    [] -> []
  | (l,p)::bs' ->
      (l, remove_stars_aug p)::
      (remove_stars_branches bs')

and remove_stars_aug {A.st_data = d; A.st_structure = p} = {A.st_data = d; A.st_structure = remove_stars_exp p}

and remove_stars_faug {A.func_data = d; A.func_structure = e} = {A.func_data = d; A.func_structure = remove_stars_fexp e}

and remove_stars_fexp fexp = match fexp with
    A.If(e1,e2,e3) -> A.If(remove_stars_faug e1, remove_stars_faug e2, remove_stars_faug e3)
  | A.LetIn(x,e1,e2) -> A.LetIn(x, remove_stars_faug e1, remove_stars_faug e2)
  | A.Bool _ | A.Int _ | A.Addr _ | A.Var _ -> fexp
  | A.ListE(l) -> A.ListE(List.map remove_stars_faug l)
  | A.App(es) -> A.App(List.map remove_stars_faug es)
  | A.Cons(e1,e2) -> A.Cons(remove_stars_faug e1, remove_stars_faug e2)
  | A.Match(e1,e2,x,xs,e3) -> A.Match(remove_stars_faug e1, remove_stars_faug e2, x, xs, remove_stars_faug e3)
  | A.Lambda(xs,e) -> A.Lambda(xs, remove_stars_faug e)
  | A.Op(e1,op,e2) -> A.Op(remove_stars_faug e1, op, remove_stars_faug e2)
  | A.CompOp(e1,cop,e2) -> A.CompOp(remove_stars_faug e1, cop, remove_stars_faug e2)
  | A.EqAddr(e1,e2) -> A.EqAddr(remove_stars_faug e1, remove_stars_faug e2)
  | A.RelOp(e1,rop,e2) -> A.RelOp(remove_stars_faug e1, rop, remove_stars_faug e2)
  | A.Tick(pot,e) -> A.Tick(remove_star pot, remove_stars_faug e)
  | A.GetTxnNum -> A.GetTxnNum
  | A.GetTxnSender -> A.GetTxnSender
  | A.Command(p) -> A.Command(remove_stars_aug p);;

(********************************************)
(* Substituting actual values for potential *)
(*   variables as well as mode variables    *)
(********************************************)

let rec getpval v sols = match sols with
    [] -> 0
  | (v',n)::sols' ->
      if v = v' then n
      else getpval v sols';;

let rec subst_pot e sols = match e with
    R.Add(e1,e2) -> R.Add(subst_pot e1 sols, subst_pot e2 sols)
  | R.Sub(e1,e2) -> R.Sub(subst_pot e1 sols, subst_pot e2 sols)
  | R.Mult(e1,e2) -> R.Mult(subst_pot e1 sols, subst_pot e2 sols)
  | R.Int(n) -> R.Int(n)
  | R.Var(v) -> R.Int(getpval v sols);;

let substitute_pot pot sols = match pot with
    A.Star -> raise InferImpossible
  | A.Arith e -> A.Arith (subst_pot e sols);;

let rec getmval v sols = match sols with
    [] -> A.Pure
  | (v',n)::sols' ->
      if v = v' then n
      else getmval v sols';;

let subst_mode m sols = match m with
    A.MVar(v) -> getmval v sols
  | _m -> raise InferImpossible;;

let substitute_mode (s,c,m) sols = (s, c, subst_mode m sols);;

let substitute_argmode arg sols = match arg with
    A.FArg a -> A.FArg a
  | A.STArg a -> A.STArg (substitute_mode a sols);;

let rec substitute_tp tp psols msols = match tp with
    A.Plus(choices) -> A.Plus(substitute_choices choices psols msols)
  | A.With(choices) -> A.With(substitute_choices choices psols msols)
  | A.Tensor(a,b,m) -> A.Tensor(substitute_tp a psols msols, substitute_tp b psols msols, subst_mode m msols)
  | A.Lolli(a,b,m) -> A.Lolli(substitute_tp a psols msols, substitute_tp b psols msols, subst_mode m msols)
  | A.One -> A.One
  | A.PayPot(pot,a) -> A.PayPot(substitute_pot pot psols, substitute_tp a psols msols)
  | A.GetPot(pot,a) -> A.GetPot(substitute_pot pot psols, substitute_tp a psols msols)
  | A.TpName(v) -> A.TpName(v)
  | A.Up(a) -> A.Up(substitute_tp a psols msols)
  | A.Down(a) -> A.Down(substitute_tp a psols msols)
  | A.FArrow(t,a) -> A.FArrow(t, substitute_tp a psols msols)
  | A.FProduct(t,a) -> A.FProduct(t, substitute_tp a psols msols)

and substitute_choices choices psols msols = match choices with
    (l,a)::choices' -> (l,substitute_tp a psols msols)::(substitute_choices choices' psols msols)
  | [] -> []

and substitute_ftp ftp psols msols = match ftp with
    A.ListTP(t,pot) -> A.ListTP(t, substitute_pot pot psols)
  | A.Integer | A.Boolean | A.Address | A.VarT _ -> ftp
  | A.Arrow(t1,t2) -> A.Arrow(substitute_ftp t1 psols msols, substitute_ftp t2 psols msols);;

let substitute_mode_list xs sols = List.map (fun c -> substitute_argmode c sols) xs;;

let rec substitute_exp exp psols msols = match exp with
    A.Fwd(x,y) -> A.Fwd(substitute_mode x msols, substitute_mode y msols)
  | A.Spawn(x,f,xs,q) -> A.Spawn(substitute_mode x msols, f, substitute_mode_list xs msols, substitute_aug q psols msols)
  | A.ExpName(x,f,xs) -> A.ExpName(substitute_mode x msols, f, substitute_mode_list xs msols)
  | A.Lab(x,k,p) -> A.Lab(substitute_mode x msols, k, substitute_aug p psols msols)
  | A.Case(x,branches) -> A.Case(substitute_mode x msols, substitute_branches branches psols msols)
  | A.Send(x,w,p) -> A.Send(substitute_mode x msols, substitute_mode w msols, substitute_aug p psols msols)
  | A.Recv(x,y,p) -> A.Recv(substitute_mode x msols, substitute_mode y msols, substitute_aug p psols msols)
  | A.Close(x) -> A.Close(substitute_mode x msols)
  | A.Wait(x,q) -> A.Wait(substitute_mode x msols, substitute_aug q psols msols)
  | A.Work(pot,p) ->
      let pot' = substitute_pot pot psols in
      A.Work(pot', substitute_aug p psols msols)
  | A.Pay(x,pot,p) ->
      let pot' = substitute_pot pot psols in
      A.Pay(substitute_mode x msols, pot', substitute_aug p psols msols)
  | A.Get(x,pot,p) ->
      let pot' = substitute_pot pot psols in
      A.Get(substitute_mode x msols, pot', substitute_aug p psols msols)
  | A.Acquire(x,y,p) -> A.Acquire(substitute_mode x msols, substitute_mode y msols, substitute_aug p psols msols)
  | A.Accept(x,y,p) -> A.Accept(substitute_mode x msols, substitute_mode y msols, substitute_aug p psols msols)
  | A.Release(x,y,p) -> A.Release(substitute_mode x msols, substitute_mode y msols, substitute_aug p psols msols)
  | A.Detach(x,y,p) -> A.Detach(substitute_mode x msols, substitute_mode y msols, substitute_aug p psols msols)
  | A.SendF(x,e,p) -> A.SendF(substitute_mode x msols, e, substitute_aug p psols msols)
  | A.RecvF(x,y,p) -> A.RecvF(substitute_mode x msols, y, substitute_aug p psols msols)
  | A.Let(x,e,p) -> A.Let(x,e, substitute_aug p psols msols)
  | A.IfS(e,p1,p2) -> A.IfS(e, substitute_aug p1 psols msols, substitute_aug p2 psols msols)
  | A.MakeChan(x,a,n,p) -> A.MakeChan(x, a, n, substitute_aug p psols msols)

and substitute_branches bs psols msols = match bs with
    [] -> []
  | (l,p)::bs' ->
      (l, substitute_aug p psols msols)::
      (substitute_branches bs' psols msols)

and substitute_aug {A.st_data = d; A.st_structure = p} psols msols = {A.st_data = d; A.st_structure = substitute_exp p psols msols}

and substitute_faug {A.func_data = d; A.func_structure = e} psols msols = {A.func_data = d; A.func_structure = substitute_fexp e psols msols}

and substitute_fexp fexp psols msols = match fexp with
    A.If(e1,e2,e3) -> A.If(substitute_faug e1 psols msols, substitute_faug e2 psols msols, substitute_faug e3 psols msols)
  | A.LetIn(x,e1,e2) -> A.LetIn(x, substitute_faug e1 psols msols, substitute_faug e2 psols msols)
  | A.Bool _ | A.Int _ | A.Addr _ | A.Var _ -> fexp
  | A.ListE(l) -> A.ListE(List.map (fun e -> substitute_faug e psols msols) l)
  | A.App(es) -> A.App(List.map (fun e -> substitute_faug e psols msols) es)
  | A.Cons(e1,e2) -> A.Cons(substitute_faug e1 psols msols, substitute_faug e2 psols msols)
  | A.Match(e1,e2,x,xs,e3) -> A.Match(substitute_faug e1 psols msols, substitute_faug e2 psols msols, x, xs, substitute_faug e3 psols msols)
  | A.Lambda(xs,e) -> A.Lambda(xs, substitute_faug e psols msols)
  | A.Op(e1,op,e2) -> A.Op(substitute_faug e1 psols msols, op, substitute_faug e2 psols msols)
  | A.CompOp(e1,cop,e2) -> A.CompOp(substitute_faug e1 psols msols, cop, substitute_faug e2 psols msols)
  | A.EqAddr(e1,e2) -> A.EqAddr(substitute_faug e1 psols msols, substitute_faug e2 psols msols)
  | A.RelOp(e1,rop,e2) -> A.RelOp(substitute_faug e1 psols msols, rop, substitute_faug e2 psols msols)
  | A.Tick(pot,e) -> A.Tick(substitute_pot pot psols, substitute_faug e psols msols)
  | A.GetTxnNum -> A.GetTxnNum
  | A.GetTxnSender -> A.GetTxnSender
  | A.Command(p) -> A.Command(substitute_aug p psols msols);;

(***************************************************************)
(* Substituting mode variables for U in channel and type modes *)
(***************************************************************)
let mnum = ref 0;;

let mfresh () =
  let m = "_m" ^ string_of_int !mnum in
  let () = mnum := !mnum + 1 in
  m;;

let unk m = match m with
    A.Unknown -> true
  | _m -> false;;

let removeU (s,c,m) = match m with
    A.Unknown -> (s, c, A.MVar(mfresh ()))
  | _m -> raise InferImpossible;;

let removeU_arg a = match a with
    A.FArg a -> A.FArg a
  | A.STArg c -> A.STArg (removeU c);;

let rec removeU_tp tp = match tp with
    A.Plus(choices) -> A.Plus(removeU_choices choices)
  | A.With(choices) -> A.With(removeU_choices choices)
  | A.Tensor(a,b,m) -> if not (unk m) then raise InferImpossible else A.Tensor(removeU_tp a, removeU_tp b, A.MVar(mfresh ()))
  | A.Lolli(a,b,m) -> if not (unk m) then raise InferImpossible else A.Lolli(removeU_tp a, removeU_tp b, A.MVar(mfresh ()))
  | A.One -> A.One
  | A.PayPot(pot,a) -> A.PayPot(pot, removeU_tp a)
  | A.GetPot(pot,a) -> A.GetPot(pot, removeU_tp a)
  | A.TpName(v) -> A.TpName(v)
  | A.Up(a) -> A.Up(removeU_tp a)
  | A.Down(a) -> A.Down(removeU_tp a)
  | A.FArrow(t,a) -> A.FArrow(t,removeU_tp a)
  | A.FProduct(t,a) -> A.FProduct(t,removeU_tp a)

and removeU_choices choices = match choices with
    (l,a)::choices' -> (l,removeU_tp a)::(removeU_choices choices')
  | [] -> [];;

let removeU_list xs = List.map (fun c -> removeU_arg c) xs;;

let rec removeU_exp exp = match exp with
    A.Fwd(x,y) -> A.Fwd(removeU x, removeU y)
  | A.Spawn(x,f,xs,q) -> A.Spawn(removeU x, f, removeU_list xs, removeU_aug q)
  | A.ExpName(x,f,xs) -> A.ExpName(removeU x, f, removeU_list xs)
  | A.Lab(x,k,p) -> A.Lab(removeU x, k, removeU_aug p)
  | A.Case(x,branches) -> A.Case(removeU x, removeU_branches branches)
  | A.Send(x,w,p) -> A.Send(removeU x, removeU w, removeU_aug p)
  | A.Recv(x,y,p) -> A.Recv(removeU x, removeU y, removeU_aug p)
  | A.Close(x) -> A.Close(removeU x)
  | A.Wait(x,q) -> A.Wait(removeU x, removeU_aug q)
  | A.Work(pot,p) -> A.Work(pot, removeU_aug p)
  | A.Pay(x,pot,p) -> A.Pay(removeU x, pot, removeU_aug p)
  | A.Get(x,pot,p) -> A.Get(removeU x, pot, removeU_aug p)
  | A.Acquire(x,y,p) -> A.Acquire(removeU x, removeU y, removeU_aug p)
  | A.Accept(x,y,p) -> A.Accept(removeU x, removeU y, removeU_aug p)
  | A.Release(x,y,p) -> A.Release(removeU x, removeU y, removeU_aug p)
  | A.Detach(x,y,p) -> A.Detach(removeU x, removeU y, removeU_aug p)
  | A.SendF(x,e,p) -> A.SendF(removeU x, e, removeU_aug p)
  | A.RecvF(x,y,p) -> A.RecvF(removeU x, y, removeU_aug p)
  | A.Let(x,e,p) -> A.Let(x,e, removeU_aug p)
  | A.IfS(e,p1,p2) -> A.IfS(e, removeU_aug p1, removeU_aug p2)
  | A.MakeChan(x,a,n,p) -> A.MakeChan(removeU x, a, n, removeU_aug p)

and removeU_branches bs = match bs with
    [] -> []
  | (l,p)::bs' ->
      (l, removeU_aug p)::
      (removeU_branches bs')

and removeU_aug {A.st_data = d; A.st_structure = p} = {A.st_data = d; A.st_structure = removeU_exp p}

and removeU_faug {A.func_data = d; A.func_structure = e} = {A.func_data = d; A.func_structure = removeU_fexp e}

and removeU_fexp fexp = match fexp with
    A.If(e1,e2,e3) -> A.If(removeU_faug e1, removeU_faug e2, removeU_faug e3)
  | A.LetIn(x,e1,e2) -> A.LetIn(x, removeU_faug e1, removeU_faug e2)
  | A.Bool _ | A.Int _ | A.Addr _ | A.Var _ -> fexp
  | A.ListE(l) -> A.ListE(List.map removeU_faug l)
  | A.App(es) -> A.App(List.map removeU_faug es)
  | A.Cons(e1,e2) -> A.Cons(removeU_faug e1, removeU_faug e2)
  | A.Match(e1,e2,x,xs,e3) -> A.Match(removeU_faug e1, removeU_faug e2, x, xs, removeU_faug e3)
  | A.Lambda(xs,e) -> A.Lambda(xs, removeU_faug e)
  | A.Op(e1,op,e2) -> A.Op(removeU_faug e1, op, removeU_faug e2)
  | A.CompOp(e1,cop,e2) -> A.CompOp(removeU_faug e1, cop, removeU_faug e2)
  | A.EqAddr(e1,e2) -> A.EqAddr(removeU_faug e1, removeU_faug e2)
  | A.RelOp(e1,rop,e2) -> A.RelOp(removeU_faug e1, rop, removeU_faug e2)
  | A.Tick(pot,e) -> A.Tick(pot, removeU_faug e)
  | A.GetTxnNum -> A.GetTxnNum
  | A.GetTxnSender -> A.GetTxnSender
  | A.Command(p) -> A.Command(removeU_aug p);;


(**************)
(* LP solving *)
(**************)
let potvar_map = ref (M.empty (module C.String));;
let modevar_map = ref (M.empty (module C.String));;

let num_vars = ref 0;;
let num_constraints = ref 0;;
let t_vars () = num_vars := !num_vars + 1;;
let t_cons () = num_constraints := !num_constraints + 1;;

let get_potvar v =
  match M.find !potvar_map v with
      None ->
        let sv = ClpS.fresh_var () in
        let () = t_vars () in
        let () = t_cons () in
        let () = ClpS.add_constr_list ~lower:0.0 [(sv, 1.0)] in
        let () = potvar_map := M.add_exn !potvar_map ~key:v ~data:sv in
        sv
    | Some sv -> sv;;

let get_modevar v =
  match M.find !modevar_map v with
      None ->
        let sv = ClpS.fresh_var () in
        let () = t_vars () in
        let () = t_cons () in
        let () = ClpS.add_constr_list ~lower:0.0 ~upper:3.0 [(sv, 1.0)] in
        let () = modevar_map := M.add_exn !modevar_map ~key:v ~data:sv in
        sv
    | Some sv -> sv;;

type entry =
    Var of int * string
  | Const of int;;

let get_entry p = match p with
    R.Mult(R.Int(n), R.Var(v)) -> Var(n, v)
  | R.Mult(R.Int(n), R.Int(1)) -> Const(n)
  | _p -> raise InferImpossible;;

let rec get_expr_list e = match e with
    R.Int(0) -> (0, [])
  | R.Add(p,s) ->
      begin
        let e = get_entry p in
        let (c, l) = get_expr_list s in
        match e with
            Var (n,v) -> (c, (n,v)::l)
          | Const(n) -> (n+c, l)
      end
  | p ->
      begin
        let e = get_entry p in
        match e with
            Var(n,v) -> (0, [(n,v)])
          | Const(n) -> (n, [])
      end;;

let constr_list l = List.map (fun (n, v) -> (get_potvar v, float_of_int n)) l;;

let add_eq_constr n l =
  if List.length l = 0 then ()
  else
    let cl = constr_list l in
    let neg_n = float_of_int (-n) in
    let () = t_cons () in
    ClpS.add_constr_list ~lower:neg_n ~upper:neg_n cl;;

let add_ge_constr n l =
  if List.length l = 0 then ()
  else
    let cl = constr_list l in
    let neg_n = float_of_int (-n) in
    let () = t_cons () in
    ClpS.add_constr_list ~lower:neg_n cl;;

let eq e1 e2 =
  try (R.evaluate e1) = (R.evaluate e2)
  with R.NotClosed ->
    let en = N.normalize (R.minus e1 e2) in
    let () = if !F.verbosity >= 2 then print_string (R.pp_arith e1 ^ " = " ^ R.pp_arith e2 ^ "\n") in
    let (n, l) = get_expr_list en in
    let () = add_eq_constr n l in
    true;;

let ge e1 e2 =
  try (R.evaluate e1) >= (R.evaluate e2)
  with R.NotClosed ->
    let en = N.normalize (R.minus e1 e2) in
    let () = if !F.verbosity >= 2 then print_string (R.pp_arith e1 ^ " >= " ^ R.pp_arith e2 ^ "\n") in
    let (n, l) = get_expr_list en in
    let () = add_ge_constr n l in
    true;;

let m_eq v1 v2 =
  if v1 = v2
  then true
  else
    let sv1 = get_modevar v1 in
    let sv2 = get_modevar v2 in
    let () = if !F.verbosity >= 2 then print_string (v1 ^ " = " ^ v2 ^ "\n") in
    let () = t_cons () in
    let () = ClpS.add_constr_list ~lower:0.0 ~upper:0.0 [(sv1, 1.0); (sv2, -1.0)]
    in true;;

let modeval m = match m with
    A.Shared -> 0.0
  | A.Pure -> 1.0
  | A.Linear -> 2.0
  | A.Transaction -> 3.0
  | _m -> raise InferImpossible;;

let get_mode f = match f with
    0.0 -> A.Shared
  | 1.0 -> A.Pure
  | 2.0 -> A.Linear
  | 3.0 -> A.Transaction
  | _f -> raise InferImpossible;;

let m_eq_const v m =
  let c = modeval m in
  let sv = get_modevar v in
  let () = if !F.verbosity >= 2 then print_string (v ^ " = " ^ PP.pp_mode m ^ "\n") in
  let () = t_cons () in
  let () = ClpS.add_constr_list ~lower:c ~upper:c [(sv, 1.0)] in
  true;;

let m_eq_pair v m1 m2 =
  let c1 = modeval m1 in
  let c2 = modeval m2 in
  let min_val = min c1 c2 in
  let max_val = max c1 c2 in
  let sv = get_modevar v in
  let () = if !F.verbosity >= 2 then print_string (v ^ " = " ^ PP.pp_mode A.Pure ^ " or " ^ v ^ " = " ^ PP.pp_mode A.Shared ^ "\n") in
  let () = t_cons () in
  let () = ClpS.add_constr_list ~lower:min_val ~upper:max_val [(sv, 1.0)] in
  true;;

let min_max m1 m2 m3 =
  let c1 = modeval m1 in
  let c2 = modeval m2 in
  let c3 = modeval m3 in
  let min_val = min c1 (min c2 c3) in
  let max_val = max c1 (max c2 c3) in
  (min_val, max_val);;

let m_lin v =
  let (min_val, max_val) = min_max A.Pure A.Linear A.Transaction in
  let sv = get_modevar v in
  let () = if !F.verbosity >= 2 then print_string (v ^ " = " ^ PP.pp_mode A.Pure ^ " or " ^ v ^ " = " ^ PP.pp_mode A.Linear ^ " or " ^ v ^ " = " ^ PP.pp_mode A.Transaction ^ "\n") in
  let () = t_cons () in
  let () = ClpS.add_constr_list ~lower:min_val ~upper:max_val [(sv, 1.0)] in
  true;;

let get_potvarlist () =
  M.fold_right !potvar_map ~init:[] ~f:(fun ~key:k ~data:_v l -> k::l);;

let get_modevarlist () =
  M.fold_right !modevar_map ~init:[] ~f:(fun ~key:k ~data:_v l -> k::l);;

let get_solution () =
  let vs = get_potvarlist () in
  let ms = get_modevarlist () in
  (List.map (fun v -> (v, int_of_float (ClpS.get_solution (get_potvar v)))) vs,
  List.map (fun m -> (m, get_mode (ClpS.get_solution (get_modevar m)))) ms);;

let rec print_pot_solution sols =
  match sols with
      [] -> ()
    | (v,n)::sols' ->
        let () = print_string (v ^ " = " ^ string_of_int n ^ "\n") in
        print_pot_solution sols';;

let rec print_mode_solution sols =
  match sols with
      [] -> ()
    | (m,n)::sols' ->
        let () = print_string (m ^ " = " ^ PP.pp_mode n ^ "\n") in
        print_mode_solution sols';;

let reset () =
  let () = potvar_map := M.empty (module C.String) in
  let () = modevar_map := M.empty (module C.String) in
  let () = mnum := 0 in
  let () = vnum := 0 in
  let () = ClpS.reset () in
  ();;

let print_stats () =
  let () = print_string ("# Vars = " ^ string_of_int !num_vars ^ "\n") in
  let () = print_string ("# Constraints = " ^ string_of_int !num_constraints ^ "\n") in
  ();;

let solve_and_print () =
  let res = ClpS.first_solve () in
  match res with
      S.Feasible ->
        let (psols, msols) = get_solution () in
        let () = if !F.verbosity >= 2 then print_pot_solution psols in
        let () = if !F.verbosity >= 2 then print_mode_solution msols in
        (psols, msols)
    | S.Infeasible ->
        let () = if !F.verbosity >= 1 then print_string ("Infeasible LP!\n") in
        raise ErrorMsg.Error;;
