module T = Typecheck
module P = Print

let rec getType (ctx : T.context) (x : string) =
        match ctx with
                [] -> raise (T.TypeError (Printf.sprintf "Unbound variable %s" x))
           |    (y, tp)::xs -> if x = y then tp else getType xs x
 

let var = ref 0

let fresh () : Ast.ocamlTP = 
        let _ = var := !var + 1 in
        let res : int = !var in 
        let a : string = Printf.sprintf "v%d" res in VarT(a)

let reset () = (var := 0)


       
let rec unify_exp (ctx : T.context) (e : Ast.expr) (t : Ast.ocamlTP) : (Ast.ocamlTP * Ast.ocamlTP) list= 
        match e with
                If(e1, e2, e3) ->
                        (unify_exp ctx e1 Ast.Boolean) @ (unify_exp ctx e2 t) @ (unify_exp ctx e3 t)
        |       LetIn(x, e1, e2) -> let t1 = fresh () in
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
        |       App(l) -> (match l with
                                 [] -> raise (T.TypeError "Impossible")
                        |       [x] -> raise (T.TypeError "Impossible")
                   |  x1::rest -> let resType = fresh () in
                                       let (constraints, result) = unify_app ctx resType rest 
                                       in constraints @ (unify_exp ctx x1 result) @ [(t, resType)])
                        
        |       Cons(x, xs) -> let t1 = fresh () in  
                                                     let res = (unify_exp ctx x t1) @ 
                                                     (unify_exp ctx xs (Ast.ListTP(t1))) @
                                                     [(t, Ast.ListTP(t1))] in
                                                    
                                                     res
        |       Match(e1, e2, x, xs, e3) -> let t1 = fresh () in
                                        (unify_exp ctx e1 (Ast.ListTP(t1))) @ (unify_exp ctx e2 t) @
                                        (unify_exp ((x, t1)::(xs, Ast.ListTP(t1))::ctx) e3 t)
        |       Lambda(args, e) -> (let t1 = fresh () in
                                   let t2 = fresh () in
                                        match args with
                                        Ast.Single(x) -> (unify_exp ((x, t1)::ctx) e t2) @
                                                                        [(t, Ast.Arrow(t1, t2))]
                                        | Ast.Curry(x, xs) ->
                                                        (unify_exp ((x, t1)::ctx)
                                                        (Ast.Lambda(xs, e)) t2) @
                                                                [(t, Ast.Arrow(t1, t2))])

        |       Op(e1, _, e2) -> (unify_exp ctx e1 Ast.Integer) @ (unify_exp ctx e2 Ast.Integer)
                                 @ [(t, Ast.Integer)]
        |       CompOp(e1, _, e2) -> (unify_exp ctx e1 Ast.Integer) @ (unify_exp ctx e2 Ast.Integer)
                                 @ [(t, Ast.Boolean)]
        |       RelOp(e1, _, e2) -> (unify_exp ctx e1 Ast.Boolean) @ (unify_exp ctx e2 Ast.Boolean)
                                 @ [(t, Ast.Boolean)]
and unify_app ctx resType rest = 
        match rest with
                [] ->  raise (T.TypeError "Impossible")
       |       [x] ->  let argVar = fresh () in (unify_exp ctx x argVar, Ast.Arrow(argVar, resType))
       |     x::xs ->  let argVar = fresh () in 
                       let (constraints, result) = unify_app ctx resType xs in
                       (constraints @ (unify_exp ctx x argVar), Ast.Arrow(argVar, result))
       
