module A = Ast
module R = Arith
module N = Normalize

(* removing stars from types and expressions *)
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

(* Replace stars in potential annotations with variables in types and expressions *)
let rec remove_stars_tps dcls = match dcls with
    [] -> []
  | {A.declaration = A.TpDef(v,a); A.decl_extent = ext}::dcls' ->
      let a' = remove_stars_tp a in
      {A.declaration = A.TpDef(v,a'); A.decl_extent = ext}::(remove_stars_tps dcls')
  | ({A.declaration = A.Pragma _; A.decl_extent = _ext} as dcl)::dcls' -> dcl::(remove_stars_tps dcls')
  | ({A.declaration = A.TpEq _; A.decl_extent = _ext} as dcl)::dcls' -> dcl::(remove_stars_tps dcls')
  | ({A.declaration = A.ExpDecDef _; A.decl_extent = _ext} as dcl)::dcls' -> dcl::(remove_stars_tps dcls')
  | ({A.declaration = A.Exec _; A.decl_extent = _ext} as dcl)::dcls' -> dcl::(remove_stars_tps dcls');;

let rec remove_stars_exps dcls = match dcls with
    [] -> []
  | {A.declaration = A.ExpDecDef(f,(ctx,pot,(z,c)),p); A.decl_extent = ext}::dcls' ->
    let remove_list = List.map (fun (x,a) -> (x, remove_stars_tp a)) in
    let {A.shared = sdelta; A.linear = ldelta; A.ordered = odelta} = ctx in
    let sdelta' = remove_list sdelta in
    let ldelta' = remove_list ldelta in
    let odelta' = remove_list odelta in
    let ctx' = {A.shared = sdelta'; A.linear = ldelta'; A.ordered = odelta'} in
    let pot' = remove_star pot in
    let zc' = (z, remove_stars_tp c) in
    let p' = remove_stars_exp p in
    {A.declaration = A.ExpDecDef(f,(ctx',pot',zc'),p'); A.decl_extent = ext}::(remove_stars_exps dcls')
  | ({A.declaration = A.Pragma _; A.decl_extent = _ext} as dcl)::dcls' -> dcl::(remove_stars_exps dcls')
  | ({A.declaration = A.TpEq _; A.decl_extent = _ext} as dcl)::dcls' -> dcl::(remove_stars_exps dcls')
  | ({A.declaration = A.TpDef _; A.decl_extent = _ext} as dcl)::dcls' -> dcl::(remove_stars_exps dcls')
  | ({A.declaration = A.Exec _; A.decl_extent = _ext} as dcl)::dcls' -> dcl::(remove_stars_exps dcls');;

let remove_stars env =
  let env = remove_stars_tps env in
  let env = remove_stars_exps env in
  env;;


let eq e1 e2 =
  try (R.evaluate e1) = (R.evaluate e2)
  with R.NotClosed ->
    let () = print_string (R.pp_arith e1 ^ " = " ^ R.pp_arith e2 ^ "\n") in
    let en = N.normalize (R.minus e1 e2) in
    let () = print_string (R.pp_arith en ^ " = 0 \n") in
    true;;

let ge e1 e2 =
  try (R.evaluate e1) >= (R.evaluate e2)
  with R.NotClosed ->
    let () = print_string (R.pp_arith e1 ^ " >= " ^ R.pp_arith e2 ^ "\n") in
    let en = N.normalize (R.minus e1 e2) in
    let () = print_string (R.pp_arith en ^ " >= 0 \n") in
    true;;