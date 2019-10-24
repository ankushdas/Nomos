module T = Typecheck

exception TypeError of string


let rec getType (ctx : T.context) (x : string) =
        match ctx with
                [] -> raise (TypeError (Printf.sprintf "Unbound variable %s" x))
           |    (y, tp)::xs -> if x = y then tp else getType xs x
 

let var = ref 0

let fresh () : Ast.ocamlTP = 
        let _ = var := !var + 1 in
        let res : int = !var in 
        let a : string = Printf.sprintf "v%d" res in Var(a)

let reset () = (var := 0)

        
let rec unify_exp (ctx : T.context) (e : Ast.expr) (t : Ast.ocamlTP) : (Ast.ocamlTP * Ast.ocamlTP) list= 
        match e with
                If(e1, e2, e3) ->
                        (unify_exp ctx e1 Ast.Boolean) @ (unify_exp ctx e2 t) @ (unify_exp ctx e3 t)
        |       LetIn(Binding(x, e1, _), e2) -> let t1 = fresh () in
                                                (unify_exp ctx e1 t1) @ (unify_exp ((x, t1)::ctx) e2 t)
        |       Bool(_)  -> [(t, Ast.Boolean)]
        |       Int(_)   -> [(t, Ast.Integer)]
        |       Var(x)   -> let t1 = getType ctx x in [(t, t1)]
        |       List(l)  -> (let t1 = fresh () in
                                match l with
                                        [] -> [(t, Ast.ListTP(t1))]
                                |    x::xs -> (unify_exp ctx x t1) 
                                                @ (unify_exp ctx (Ast.List(xs)) (Ast.ListTP(t1)))
                                                @ [(t, Ast.ListTP(t1))])
        |       App((e1, _), (e2, _)) -> let t1 = fresh () in
                                         (unify_exp ctx e1 (Ast.Arrow(t1, t))) @ 
                                         (unify_exp ctx e2 t1)
        |       Cons(x, xs) -> let t1 = fresh () in  (unify_exp ctx x t1) @ 
                                                     (unify_exp ctx xs (Ast.ListTP(t1))) @
                                                     [(t, Ast.ListTP(t1))]
        |       Match((e1, _), e2, x, xs, e3) -> let t1 = fresh () in
                                (unify_exp ctx e1 (Ast.ListTP(t1))) @ (unify_exp ctx e2 t) @
                                        (unify_exp ((x, t1)::(xs, Ast.ListTP(t1))::ctx) e3 t)
        |       Lambda(args, e) -> (let t1 = fresh () in
                                   let t2 = fresh () in
                                        match args with
                                        Ast.Single(x, _) -> (unify_exp ((x, t1)::ctx) e t2) @
                                                                        [(t, Ast.Arrow(t1, t2))]
                                        | Ast.Curry((x, _), xs) -> 
                                                        (unify_exp ((x, t1)::ctx)
                                                        (Ast.Lambda(xs, e)) t2) @
                                                                [(t, Ast.Arrow(t1, t2))])
        |       Op(e1, _, e2) -> (unify_exp ctx e1 Ast.Integer) @ (unify_exp ctx e2 Ast.Integer)
                                 @ [(t, Ast.Integer)]
        |       CompOp(e1, _, e2) -> (unify_exp ctx e1 Ast.Integer) @ (unify_exp ctx e2 Ast.Integer)
                                 @ [(t, Ast.Boolean)]
        |       RelOp(e1, _, e2) -> (unify_exp ctx e1 Ast.Boolean) @ (unify_exp ctx e2 Ast.Boolean)
                                 @ [(t, Ast.Boolean)]
                                        
                                                                                        
(*let inferType (e : Ast.expr) = 
        let a = fresh () in
        let l = unify_exp [] e a in
        ()
*)



(*let rec unify_exp (ctx : context) (e : Ast.typedExpr)  = 
        match e with
                BoolT(_, t) -> [(t, Ast.Boolean)]
        |       IntT(_, t) ->  [(t, Ast.Integer)]
        |       
*)
(*let rec transform (e : Ast.expr) : Ast.typedExpr = 
        let a : Ast.ocamlTP = Var(fresh ()) in
        match e with
        If(e1, e2, e3) -> IfT(transform e1, transform e2, transform e3, a) 
        | LetIn (Binding(x,e,t), e1) -> LetInT(BindingT(x, transform e), transform e1, a)
        | Bool v -> BoolT(v, a)
        | Int v  -> IntT(v, a)
        | Var(x) -> VarT(x, a)
        | List (l) -> ListT(l, a) 
        | App ((e1, t1), (e2, t2)) -> AppT(transform e1, transform e2, a) 
        | Cons (x, xs) -> ConsT(transform x, transform xs, a) 
        | Match ((e1,t1), e2, id1, id2, e3) ->  MatchT(transform e1, transform e2, id1, id2, transform e3, a)
        | Lambda(l, e) -> LambdaT(transformArgs l, transform e, a) 
        | Op (e1, op, e2) -> OpT(transform e1, op, transform e2, a) 
        | Ast.CompOp (e1, op, e2) -> CompOpT(transform e1, op, transform e2, a)
        | Ast.RelOp (e1, op, e2) -> RelOpT(transform e1, op, transform e2, a) 
and transformArgs l = let b : Ast.ocamlTP = Var(fresh ()) in
                        match l with
                                Ast.Single(x, _) -> Ast.Single(x, b)
                        | Ast.Curry((x,t), xs) -> Ast.Curry((x,b), transformArgs xs)
*)
