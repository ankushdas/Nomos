(*let rec getType (ctx : context) (x : string) =
        match ctx with
                [] -> raise (TypeError (Printf.sprintf "Unbound variable %s" x))
           |    (y, tp)::xs -> if x = y then tp else getType xs x


let rec unify_exp (ctx : context) (e : Ast.expr) (t : Ast.ocamlTP) : bool = 
        match e with
        If(e1, e2, e3) -> let t1 = typecheck ctx e1 Ast.Boolean in
                          let t2 = typecheck ctx e2 t in
                          let t3 = typecheck ctx e3 t in
                          if t1 && t2 && t3 then true else raise (TypeError (format_err e t))

        | LetIn (Ast.Binding(var, expr, typ), e) -> if (typecheck ctx expr typ) &&
                                              (typecheck ((var, typ)::ctx) e t)
                                              then true
                                              else raise (TypeError (format_err e t))
        | Bool _ -> if t = Ast.Boolean then true else raise (TypeError (format_err e t))
        | Int _  -> if t = Ast.Integer then true else raise (TypeError (format_err e t))
        | Var(x) -> if t = (getType ctx x) then true else raise (TypeError (format_err e t))
        | List (l) -> (match (t, l) with
                        (ListTP(t1), []) -> true
                     |  (ListTP(t1), e::es) -> if (typecheck ctx e t1) && (typecheck ctx (List(es)) t)
                                               then true else raise (TypeError (format_err e t))
                     |  _                   -> raise (TypeError (format_err e t)))
        | App ((e1, t1), (e2, t2)) -> (match t1 with
                                        Arrow (t3, t4) -> if (typecheck ctx e1 t1) &&
                                                             (typecheck ctx e2 t2) &&
                                                             (type_equals t2 t3) && (type_equals t4 t)
                                                          then
                                                             true
                                                          else
                                                             raise (TypeError (format_err e t))
                                        | _            -> raise (TypeError (format_err e t)))
        | Cons (x, xs) -> (match t with
                          ListTP(t1) -> if (typecheck ctx x t1) && (typecheck ctx xs t)
                                        then true else raise (TypeError (format_err e t))
                       |  _          -> raise (TypeError (format_err e t)))
        | Match ((e1,t1), e2, id1, id2, e3) -> (* Should add check for duplicate variables *)
                                                        (match t1 with
                                                        ListTP(t2) -> if (typecheck ctx e1 t1)
                                                                &&
                                                                (typecheck ctx e2 t)
                                                                && (typecheck ((id1, t2)::(id2, t1)::ctx)
                                                                                   e3 t)
                                                                then true
                                                                else raise (TypeError (format_err e t))
                                                        | _        -> raise (TypeError (format_err e t)))
        | Lambda(l, e) -> (match t with
                                        Arrow(t1, t2) -> 
                                                         let (len, ctx') = addArglist l ctx in
                                                         if (typecheck ctx' e (get_result_type t))
                                                         then true
                                                         else raise (TypeError (format_err e t))
                                        | _        -> raise (TypeError (format_err e t)))
        | Op (e1, op, e2) -> let a : bool = typecheck ctx e1 t in
                             let b : bool = typecheck ctx e2 t in
                             if a && b && (type_equals t Ast.Integer)
                             then true
                             else raise (TypeError (format_err e t))
        | Ast.CompOp (e1, op, e2) ->
                                 let a : bool = typecheck ctx e1 (Ast.Integer) in
                                 let b : bool = typecheck ctx e2 (Ast.Integer) in
                                 if a && b && (type_equals t Ast.Boolean)
                                 then true
                                 else raise (TypeError (format_err e t))

        | Ast.RelOp (e1, op, e2) -> let a : bool = typecheck ctx e1 (Ast.Boolean) in
                                 let b : bool = typecheck ctx e2 (Ast.Boolean) in
                                 if a && b && (type_equals t Ast.Boolean)
                                 then true
                                 else raise (TypeError (format_err e t))

and addArglist l ctx = match l with
                                Ast.Single(x, t1) -> (1, (x,t1)::ctx)
                            |   Ast.Curry((x,t1), rest) -> 
                                            let
                                                (len, ctx') = addArglist rest ctx
                                            in
                                                (len + 1, (x,t1)::ctx')
*)
let var = ref 0

let fresh () : string = 
        let _ = var := !var + 1 in
        let res : int = !var in 
        let a : string = Printf.sprintf "v%d" res in a

let rec transform (e : Ast.expr) : Ast.typedExpr = 
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
        | Lambda(l, e) -> LambdaT(l, transform e, a) 
        | Op (e1, op, e2) -> OpT(transform e1, op, transform e2, a) 
        | Ast.CompOp (e1, op, e2) -> CompOpT(transform e1, op, transform e2, a)
        | Ast.RelOp (e1, op, e2) -> RelOpT(transform e1, op, transform e2, a) 

let inferType (e : Ast.expr) = 
        let e' = transform e in ()
