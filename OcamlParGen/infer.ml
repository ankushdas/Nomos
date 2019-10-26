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

let rec addType (e : Ast.untyped_expr) : Ast.typed_expr = 
        let typeVar = fresh () in
        match e.structure with
                If(e1, e2, e3) -> {structure = If(addType e1, addType e2, addType e3); data = typeVar}
        |       LetIn(x, e1, e2) -> {structure = LetIn(x, addType e1, addType e2); data = typeVar} 
        |       Bool(x)  -> {structure = Bool(x); data = typeVar}
        |       Int(x)  -> {structure = Int(x); data = typeVar}
        |       Var(x)   -> {structure = Var(x); data = typeVar}
        |       List(l)  -> {structure = List(List.map addType l); data = typeVar} 
        |       App(l) -> {structure = App(List.map addType l); data = typeVar} 
                        
        |       Cons(x, xs) -> {structure = Cons(addType x, addType xs); data = typeVar}
        |       Match(e1, e2, x, xs, e3) -> {structure = Match(addType e1, addType e2, x, xs, addType e3); data = typeVar} 
        |       Lambda(args, e) -> {structure = Lambda(args, addType e); data = typeVar} 
        |       Op(e1, o, e2) -> {structure = Op(addType e1, o, addType e2); data =  typeVar}
        |       CompOp(e1, o, e2) -> {structure = CompOp(addType e1, o, addType e2); data =  typeVar} 
        |       RelOp(e1, o, e2) -> {structure = RelOp(addType e1, o, addType e2); data = typeVar} 


       
let rec unify_exp (ctx : T.context) (e : Ast.typed_expr) : (Ast.ocamlTP * Ast.ocamlTP) list= 
        let t = e.data in
        match e.structure with
                If(e1, e2, e3) -> [(e1.data, Ast.Boolean); (e2.data, t); (e3.data, t)] @
                        (unify_exp ctx e1) @ (unify_exp ctx e2) @ (unify_exp ctx e3)
        |       LetIn(x, e1, e2) -> [(e2.data, t)] @ (unify_exp ctx e1) @ (unify_exp ((x, e1.data)::ctx) e2)
        |       Bool(_)  -> [(t, Ast.Boolean)]
        |       Int(_)   -> [(t, Ast.Integer)]
        |       Var(x)   -> let t1 = getType ctx x in [(t, t1)]
        |       List(l)  -> (let t1 = fresh () in
                            match l with
                                [] -> [(t, Ast.ListTP(t1))]
                        | x::xs    -> [(t, Ast.ListTP(t1))] @ (constrain_list l t1))
        |       App(l) -> 
                        (match l with
                                 [] -> raise (T.TypeError "Impossible")
                        |       [x] -> raise (T.TypeError "Impossible")
                        |  x1::rest -> let resType = fresh () in
                                       let (constraints, result) = unify_app ctx resType rest
                                       in constraints @ [(x1.data, result); (t, resType)])
                        
        |       Cons(x, xs) -> (unify_exp ctx x) @  (unify_exp ctx xs) @ [(Ast.ListTP(x.data), xs.data)]
        |       Match(e1, e2, x, xs, e3) -> let t1 = fresh () in
                                [(e1.data, Ast.ListTP(t1)); (e2.data, t); (e3.data, t)] @ (unify_exp ctx e1)
                                        @ (unify_exp ctx e2) @ (unify_exp ((x, t1)::(xs, Ast.ListTP(t1))::ctx) e3)
        |       Lambda(args, e) ->  let t1 = fresh () in
                                    let t2 = fresh () in
                                    (match args with
                                        Ast.Single(x) -> (unify_exp ((x, t1)::ctx) e) @
                                                                        [(t, Ast.Arrow(t1, e.data))]
                                        | Ast.Curry(x, xs) ->
                                                        (unify_exp ((x, t1)::ctx)
                                                        {structure = Ast.Lambda(xs, e); data = t2}) @
                                                                [(t, Ast.Arrow(t1, t2))])

        |       Op(e1, _, e2) -> [(t, Ast.Integer); (e1.data, Ast.Integer); (e2.data, Ast.Integer)]
        |       CompOp(e1, _, e2) -> [(t, Ast.Boolean); (e1.data, Ast.Integer); (e2.data, Ast.Integer)]
        |       RelOp(e1, _, e2) -> [(t, Ast.Boolean); (e1.data, Ast.Boolean); (e2.data, Ast.Boolean)]

and constrain_list l t = 
        match l with
                [] -> raise (T.TypeError "Impossible")
        |      [e] -> [(e.data, t)]
        |    e::es -> (e.data, t) :: (constrain_list es t)

and unify_app ctx resType rest = 
        match rest with
                [] ->  raise (T.TypeError "Impossible")
       |       [x] ->  (unify_exp ctx x, Ast.Arrow(x.data, resType))
       |     x::xs ->  let (constraints, result) = unify_app ctx resType xs in
                       (constraints @ (unify_exp ctx x), Ast.Arrow(x.data, result))
       
