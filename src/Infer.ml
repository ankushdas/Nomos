module A = Ast
module R = Arith
module N = Normalize
module S = Solver
module ClpS = S.Clp (S.Clp_std_options)
module C = Core
module M = C.Map

(* removing stars from potential annotations *)
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
  | A.Tensor(a,b) -> A.Tensor(remove_stars_tp a, remove_stars_tp b)
  | A.Lolli(a,b) -> A.Lolli(remove_stars_tp a, remove_stars_tp b)
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

let index_map = ref (M.empty (module C.String));;

let get_var v =
  match M.find !index_map v with
      None ->
        let sv = ClpS.fresh_var () in
        let () = ClpS.add_constr_list ~lower:0.0 [(sv, 1.0)] in
        let () = index_map := M.add_exn !index_map ~key:v ~data:sv in
        sv
    | Some sv -> sv;;

exception InferImpossible

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

let constr_list l = List.map (fun (n, v) -> (get_var v, float_of_int n)) l;;

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
    let (n, l) = get_expr_list en in
    let () = add_eq_constr n l in
    true;;

let ge e1 e2 =
  try (R.evaluate e1) >= (R.evaluate e2)
  with R.NotClosed ->
    let en = N.normalize (R.minus e1 e2) in
    let (n, l) = get_expr_list en in
    let () = add_ge_constr n l in
    true;;

let get_varlist () =
  M.fold_right !index_map ~init:[] ~f:(fun ~key:k ~data:_v l -> k::l);;

let get_solution () =
  let vs = get_varlist () in
  List.map (fun v -> (v, ClpS.get_solution (get_var v))) vs;;

let rec print_solution sols =
  match sols with
      [] -> ()
    | (v,n)::sols' ->
        let () = print_string (v ^ " = " ^ string_of_float n ^ "\n") in
        print_solution sols';;

let reset () =
  let () = index_map := M.empty (module C.String) in
  ClpS.reset ();;

let solve_and_print () =
  let res = ClpS.first_solve () in
  match res with
      S.Feasible ->
        let sols = get_solution () in
        let () = print_solution sols in
        true
    | S.Infeasible ->
        let () = print_string ("Infeasible LP!\n") in
        raise ErrorMsg.Error;;