module A = Ast
module R = Arith
module N = Normalize
module S = Solver
module C = Core
module M = C.Map
module PP = Pprint
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

and remove_stars_choices choices = match choices with
    (l,a)::choices' -> (l,remove_stars_tp a)::(remove_stars_choices choices')
  | [] -> [];;

let rec remove_stars_exp exp = match exp with
    A.Fwd(x,y) -> A.Fwd(x,y)
  | A.Spawn(x,f,xs,q) -> A.Spawn(x,f,xs, remove_stars_exp q)
  | A.ExpName(x,f,xs) -> A.ExpName(x,f,xs)
  | A.Lab(x,k,p) -> A.Lab(x,k, remove_stars_exp p)
  | A.Case(x,branches) -> A.Case(x, remove_stars_branches branches)
  | A.Send(x,w,p) -> A.Send(x,w, remove_stars_exp p)
  | A.Recv(x,y,p) -> A.Recv(x,y, remove_stars_exp p)
  | A.Close(x) -> A.Close(x)
  | A.Wait(x,q) -> A.Wait(x, remove_stars_exp q)
  | A.Work(pot,p) ->
      let pot' = remove_star pot in
      A.Work(pot', remove_stars_exp p)
  | A.Pay(x,pot,p) ->
      let pot' = remove_star pot in
      A.Pay(x, pot', remove_stars_exp p)
  | A.Get(x,pot,p) ->
      let pot' = remove_star pot in
      A.Get(x, pot', remove_stars_exp p)
  | A.Acquire(x,y,p) -> A.Acquire(x,y, remove_stars_exp p)
  | A.Accept(x,y,p) -> A.Accept(x,y, remove_stars_exp p)
  | A.Release(x,y,p) -> A.Release(x,y, remove_stars_exp p)
  | A.Detach(x,y,p) -> A.Detach(x,y, remove_stars_exp p)
  | A.Marked(marked_p) -> Marked(remove_stars_exp (Mark.data marked_p), Mark.ext marked_p)

and remove_stars_branches bs = match bs with
    [] -> []
  | {lab_exp = (l,p); exp_extent = ext}::bs' ->
      {lab_exp = (l, remove_stars_exp p); exp_extent = ext}::
      (remove_stars_branches bs');;

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
    A.Var(v) -> getmval v sols
  | _m -> raise InferImpossible;;

let substitute_mode (c,m) sols = (c, subst_mode m sols);;
  
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

and substitute_choices choices psols msols = match choices with
    (l,a)::choices' -> (l,substitute_tp a psols msols)::(substitute_choices choices' psols msols)
  | [] -> [];;

let substitute_mode_list xs sols = List.map (fun c -> substitute_mode c sols) xs;;

let rec substitute_exp exp psols msols = match exp with
    A.Fwd(x,y) -> A.Fwd(substitute_mode x msols, substitute_mode y msols)
  | A.Spawn(x,f,xs,q) -> A.Spawn(substitute_mode x msols, f, substitute_mode_list xs msols, substitute_exp q psols msols)
  | A.ExpName(x,f,xs) -> A.ExpName(substitute_mode x msols, f, substitute_mode_list xs msols)
  | A.Lab(x,k,p) -> A.Lab(substitute_mode x msols, k, substitute_exp p psols msols)
  | A.Case(x,branches) -> A.Case(substitute_mode x msols, substitute_branches branches psols msols)
  | A.Send(x,w,p) -> A.Send(substitute_mode x msols, substitute_mode w msols, substitute_exp p psols msols)
  | A.Recv(x,y,p) -> A.Recv(substitute_mode x msols, substitute_mode y msols, substitute_exp p psols msols)
  | A.Close(x) -> A.Close(x)
  | A.Wait(x,q) -> A.Wait(substitute_mode x msols, substitute_exp q psols msols)
  | A.Work(pot,p) ->
      let pot' = substitute_pot pot psols in
      A.Work(pot', substitute_exp p psols msols)
  | A.Pay(x,pot,p) ->
      let pot' = substitute_pot pot psols in
      A.Pay(substitute_mode x msols, pot', substitute_exp p psols msols)
  | A.Get(x,pot,p) ->
      let pot' = substitute_pot pot psols in
      A.Get(substitute_mode x msols, pot', substitute_exp p psols msols)
  | A.Acquire(x,y,p) -> A.Acquire(substitute_mode x msols, substitute_mode y msols, substitute_exp p psols msols)
  | A.Accept(x,y,p) -> A.Accept(substitute_mode x msols, substitute_mode y msols, substitute_exp p psols msols)
  | A.Release(x,y,p) -> A.Release(substitute_mode x msols, substitute_mode y msols, substitute_exp p psols msols)
  | A.Detach(x,y,p) -> A.Detach(substitute_mode x msols, substitute_mode y msols, substitute_exp p psols msols)
  | A.Marked(marked_p) -> Marked(substitute_exp (Mark.data marked_p) psols msols, Mark.ext marked_p)

and substitute_branches bs psols msols = match bs with
    [] -> []
  | {lab_exp = (l,p); exp_extent = ext}::bs' ->
      {lab_exp = (l, substitute_exp p psols msols); exp_extent = ext}::
      (substitute_branches bs' psols msols);;

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

let removeU (c,m) = match m with
    A.Unknown -> (c, A.Var(mfresh ()))
  | _m -> raise InferImpossible;;

let rec removeU_tp tp = match tp with
    A.Plus(choices) -> A.Plus(removeU_choices choices)
  | A.With(choices) -> A.With(removeU_choices choices)
  | A.Tensor(a,b,m) -> if not (unk m) then raise InferImpossible else A.Tensor(removeU_tp a, removeU_tp b, A.Var(mfresh ()))
  | A.Lolli(a,b,m) -> if not (unk m) then raise InferImpossible else A.Lolli(removeU_tp a, removeU_tp b, A.Var(mfresh ()))
  | A.One -> A.One
  | A.PayPot(pot,a) -> A.PayPot(pot, removeU_tp a)
  | A.GetPot(pot,a) -> A.GetPot(pot, removeU_tp a)
  | A.TpName(v) -> A.TpName(v)
  | A.Up(a) -> A.Up(removeU_tp a)
  | A.Down(a) -> A.Down(removeU_tp a)

and removeU_choices choices = match choices with
    (l,a)::choices' -> (l,removeU_tp a)::(removeU_choices choices')
  | [] -> [];;

let removeU_list xs = List.map (fun c -> removeU c) xs;;

let rec removeU_exp exp = match exp with
    A.Fwd(x,y) -> A.Fwd(removeU x, removeU y)
  | A.Spawn(x,f,xs,q) -> A.Spawn(removeU x, f, removeU_list xs, removeU_exp q)
  | A.ExpName(x,f,xs) -> A.ExpName(removeU x, f, removeU_list xs)
  | A.Lab(x,k,p) -> A.Lab(removeU x, k, removeU_exp p)
  | A.Case(x,branches) -> A.Case(removeU x, removeU_branches branches)
  | A.Send(x,w,p) -> A.Send(removeU x, removeU w, removeU_exp p)
  | A.Recv(x,y,p) -> A.Recv(removeU x, removeU y, removeU_exp p)
  | A.Close(x) -> A.Close(removeU x)
  | A.Wait(x,q) -> A.Wait(removeU x, removeU_exp q)
  | A.Work(pot,p) -> A.Work(pot, removeU_exp p)
  | A.Pay(x,pot,p) -> A.Pay(removeU x, pot, removeU_exp p)
  | A.Get(x,pot,p) -> A.Get(removeU x, pot, removeU_exp p)
  | A.Acquire(x,y,p) -> A.Acquire(removeU x, removeU y, removeU_exp p)
  | A.Accept(x,y,p) -> A.Accept(removeU x, removeU y, removeU_exp p)
  | A.Release(x,y,p) -> A.Release(removeU x, removeU y, removeU_exp p)
  | A.Detach(x,y,p) -> A.Detach(removeU x, removeU y, removeU_exp p)
  | A.Marked(marked_p) -> Marked(removeU_exp (Mark.data marked_p), Mark.ext marked_p)

and removeU_branches bs = match bs with
    [] -> []
  | {lab_exp = (l,p); exp_extent = ext}::bs' ->
      {lab_exp = (l, removeU_exp p); exp_extent = ext}::
      (removeU_branches bs');;

(**************)
(* LP solving *)
(**************)
let potvar_map = ref (M.empty (module C.String));;
let modevar_map = ref (M.empty (module C.String));;

let get_potvar v =
  match M.find !potvar_map v with
      None ->
        let sv = ClpS.fresh_var () in
        let () = ClpS.add_constr_list ~lower:0.0 [(sv, 1.0)] in
        let () = potvar_map := M.add_exn !potvar_map ~key:v ~data:sv in
        sv
    | Some sv -> sv;;

let get_modevar v =
  match M.find !modevar_map v with
      None ->
        let sv = ClpS.fresh_var () in
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
    ClpS.add_constr_list ~lower:neg_n ~upper:neg_n cl;;

let add_ge_constr n l =
  if List.length l = 0 then ()
  else
    let cl = constr_list l in
    let neg_n = float_of_int (-n) in
    ClpS.add_constr_list ~lower:neg_n cl;;

let eq e1 e2 =
  try (R.evaluate e1) = (R.evaluate e2)
  with R.NotClosed ->
    let en = N.normalize (R.minus e1 e2) in
    let () = if !Flags.verbosity >= 2 then print_string (R.pp_arith e1 ^ " = " ^ R.pp_arith e2 ^ "\n") in
    let (n, l) = get_expr_list en in
    let () = add_eq_constr n l in
    true;;

let ge e1 e2 =
  try (R.evaluate e1) >= (R.evaluate e2)
  with R.NotClosed ->
    let en = N.normalize (R.minus e1 e2) in
    let () = if !Flags.verbosity >= 2 then print_string (R.pp_arith e1 ^ " >= " ^ R.pp_arith e2 ^ "\n") in
    let (n, l) = get_expr_list en in
    let () = add_ge_constr n l in
    true;;

let m_eq v1 v2 =
  if v1 = v2
  then true
  else
    let sv1 = get_modevar v1 in
    let sv2 = get_modevar v2 in
    let () = if !Flags.verbosity >= 2 then print_string (v1 ^ " = " ^ v2 ^ "\n") in
    let () = ClpS.add_constr_list ~lower:0.0 ~upper:0.0 [(sv1, 1.0); (sv2, -1.0)]
    in true;;

let modeval m = match m with
    A.Pure -> 0.0
  | A.Linear -> 1.0
  | A.Transaction -> 2.0
  | A.Shared -> 3.0
  | _m -> raise InferImpossible;;

let get_mode f = match f with
    0.0 -> A.Pure
  | 1.0 -> A.Linear
  | 2.0 -> A.Transaction
  | 3.0 -> A.Shared
  | _f -> raise InferImpossible;;

let m_eq_const v m =
  let c = (modeval m) in
  let sv = get_modevar v in
  let () = if !Flags.verbosity >= 2 then print_string (v ^ " = " ^ PP.pp_mode m ^ "\n") in
  let () = ClpS.add_constr_list ~lower:c ~upper:c [(sv, 1.0)] in
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
  let () = if !Flags.verbosity >= 2 then print_string (v ^ " = " ^ PP.pp_mode A.Pure ^ " or " ^ v ^ " = " ^ PP.pp_mode A.Linear ^ " or " ^ v ^ " = " ^ PP.pp_mode A.Transaction ^ "\n") in
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
  ClpS.reset ();;

let solve_and_print () =
  let res = ClpS.first_solve () in
  match res with
      S.Feasible ->
        let (psols, msols) = get_solution () in
        let () = if !Flags.verbosity >= 2 then print_pot_solution psols in
        let () = if !Flags.verbosity >= 2 then print_mode_solution msols in
        (psols, msols)
    | S.Infeasible ->
        let () = if !Flags.verbosity >= 1 then print_string ("Infeasible LP!\n") in
        raise ErrorMsg.Error;;