module T = Typecheck
module P = Print
module U = Unify

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
                        | x::xs    -> [(t, Ast.ListTP(t1))] @ (constrain_list l t1 ctx))
        |       App(l) -> 
                        (match l with
                                 [] -> raise (T.TypeError "Impossible")
                        |       [x] -> raise (T.TypeError "Impossible")
                        |  x1::rest -> let resType = fresh () in
                                       let (constraints, result) = unify_app ctx resType rest
                                       in constraints @ [(x1.data, result); (t, resType)] @ (unify_exp ctx x1))
                        
        |       Cons(x, xs) -> (unify_exp ctx x) @  (unify_exp ctx xs) @ [(Ast.ListTP(x.data), xs.data); (xs.data, t)]
        |       Match(e1, e2, x, xs, e3) -> let t1 = fresh () in
                                [(e1.data, Ast.ListTP(t1)); (e2.data, t); (e3.data, t)] @ (unify_exp ctx e1)
                                        @ (unify_exp ctx e2) @ (unify_exp ((x, t1)::(xs, Ast.ListTP(t1))::ctx) e3)
        |       Lambda(args, e) ->   let (context', resultType) = unify_lambda args ctx e.data in
                                        (unify_exp context' e) @ [(t, resultType)]
                                        

        |       Op(e1, _, e2) -> [(t, Ast.Integer); (e1.data, Ast.Integer); (e2.data, Ast.Integer)] @ (unify_exp ctx e1) @ (unify_exp ctx e2)
        |       CompOp(e1, _, e2) -> [(t, Ast.Boolean); (e1.data, Ast.Integer); (e2.data, Ast.Integer)] @ (unify_exp ctx e1) @ (unify_exp ctx e2)
        |       RelOp(e1, _, e2) -> [(t, Ast.Boolean); (e1.data, Ast.Boolean); (e2.data, Ast.Boolean)] @ (unify_exp ctx e1) @ (unify_exp ctx e2)


and constrain_list l t ctx = 
        match l with
                [] -> raise (T.TypeError "Impossible")
        |      [e] -> [(e.data, t)] @ (unify_exp ctx e)
        |    e::es -> ((e.data, t) :: (constrain_list es t ctx)) @ (unify_exp ctx e)

and unify_app ctx resType rest = 
        match rest with
                [] ->  raise (T.TypeError "Impossible")
       |       [x] ->  (unify_exp ctx x, Ast.Arrow(x.data, resType))
       |     x::xs ->  let (constraints, result) = unify_app ctx resType xs in
                       (constraints @ (unify_exp ctx x), Ast.Arrow(x.data, result))
and unify_lambda args ctx t = match args with
                                Ast.Single(x) -> let v = fresh () in ((x, v)::ctx, Ast.Arrow(v, t))
                           | Ast.Curry(x, xs) -> let v = fresh () in 
                                                 let (ctx', result) = unify_lambda xs ctx t in
                                                 ((x, v)::ctx', Ast.Arrow(v, result))


let rec apply_sub (s : (string * Ast.ocamlTP) list) (e : Ast.typed_expr) : Ast.typed_expr = 
        let t = U.apply s (e.data) in
        match e.structure with
                If(e1, e2, e3) -> {structure = If(apply_sub s e1, apply_sub s e2, apply_sub s e3); data = t}
        |       LetIn(x, e1, e2) -> {structure = LetIn(x, apply_sub s e1, apply_sub s e2); data = t} 
        |       Bool(x)  -> {structure = Bool(x); data = t}
        |       Int(x)  -> {structure = Int(x); data = t}
        |       Var(x)   -> {structure = Var(x); data = t}
        |       List(l)  -> {structure = List(List.map (apply_sub s) l); data = t} 
        |       App(l) -> {structure = App(List.map (apply_sub s) l); data = t} 
                        
        |       Cons(x, xs) -> {structure = Cons(apply_sub s x, apply_sub s xs); data = t}
        |       Match(e1, e2, x, xs, e3) -> {structure = Match(apply_sub s e1, apply_sub s e2, x, xs, apply_sub s e3); data = t} 
        |       Lambda(args, e) -> {structure = Lambda(args, apply_sub s e); data = t} 
        |       Op(e1, o, e2) -> {structure = Op(apply_sub s e1, o, apply_sub s e2); data =  t}
        |       CompOp(e1, o, e2) -> {structure = CompOp(apply_sub s e1, o, apply_sub s e2); data =  t} 
        |       RelOp(e1, o, e2) -> {structure = RelOp(apply_sub s e1, o, apply_sub s e2); data = t} 
