type valContext = (string * Ast.value) list




let rec lookupInContext (ctx : valContext) x = 
        match ctx with
                [] -> raise (Failure "Empty context")
           | (y,v)::ys -> if x = y then v else lookupInContext ys x





let rec evaluate (ctx : valContext) (e : Ast.expr) : Ast.value = 
        match e with
        If(e1, e2, e3) -> (let ifVal = evaluate ctx e1 in
                          let thenVal = evaluate ctx e2 in
                          let elseVal = evaluate ctx e3 in
                          match ifVal with
                            Ast.BoolV(true) -> thenVal
                          | Ast.BoolV(false) -> elseVal
                          | _                -> raise (Failure "Impossible"))

        | Bool (v) ->  Ast.BoolV (v)
        | Int (v)  ->  Ast.IntV(v)
        | Var(x) -> lookupInContext ctx x 
        
        | LetIn (Ast.Binding(var, expr, _), e') -> let letVal = evaluate ctx expr in
                                                   let expVal = evaluate ((var, letVal)::ctx) e' in
                                                   expVal

        | List (l) -> (match l with
                         [] -> Ast.ListV([])
                     |   (x::xs) -> let firstVal = evaluate ctx x in
                                    let secondVal = evaluate ctx (Ast.List(xs)) in
                                      match secondVal with
                                          Ast.ListV(lst) -> Ast.ListV(firstVal::lst)
                                        | _              -> raise (Failure "Impossible"))

        | App ((e1, t1), (e2, t2)) -> (let funcVal = evaluate ctx e1 in
                                      let argVal = evaluate ctx e2 in
                                      match funcVal with
                                        Ast.LambdaV(ctx', args, expr) -> evaluateFunc ctx' args expr argVal
                                      | _                             -> raise (Failure "Impossible"))


        | Cons (x, xs) -> (let firstVal = evaluate ctx x in
                          let secondVal = evaluate ctx xs in 
                                        match secondVal with
                                          Ast.ListV(lst) -> Ast.ListV(firstVal::lst)
                                        | _              -> raise (Failure "Impossible"))




        | Match ((e1,t1), e2, id1, id2, e3) -> (let matchVal = evaluate ctx e1 in
                                               match matchVal with
                                                    Ast.ListV([]) -> evaluate ctx e2
                                                | Ast.ListV(v::vs) -> evaluate ((id1, v)::(id2, Ast.ListV(vs))::ctx) e3
                                                | _                -> raise (Failure "Impossible"))


        | Lambda(l, e) -> Ast.LambdaV(ctx, l, e)
        | Op (e1, op, e2) -> (let firstArg = evaluate ctx e1 in
                            let secondArg = evaluate ctx e2 in
                            match (firstArg, secondArg) with
                              (Ast.IntV(f), Ast.IntV(s)) ->
                            (match op with
                                "+" -> Ast.IntV(f + s)
                              | "-" -> Ast.IntV(f - s)
                              | "/" -> Ast.IntV(f / s)
                              | "*" -> Ast.IntV(f * s)
                              | _   -> raise (Failure "undefined operation"))
                            | _   -> raise (Failure "impossible"))
        | Ast.CompOp (e1, op, e2) -> (let firstArg = evaluate ctx e1 in
                            let secondArg = evaluate ctx e2 in
                            match (firstArg, secondArg) with
                              (Ast.IntV(f), Ast.IntV(s)) ->
                            (match op with
                                ">"  ->  Ast.BoolV(f > s)
                              | "<"  ->  Ast.BoolV(f < s)
                              | ">=" ->  Ast.BoolV(f >= s)
                              | "<=" ->  Ast.BoolV(f <= s)
                              | "="  ->  Ast.BoolV(f = s)
                              | "<>" ->  Ast.BoolV(f <> s)
                              | _   -> raise (Failure "undefined operation"))
                            | _   -> raise (Failure "impossible"))
        | Ast.RelOp (e1, op, e2) -> let firstArg = evaluate ctx e1 in
                            let secondArg = evaluate ctx e2 in
                            match (firstArg, secondArg) with
                              (Ast.BoolV(f), Ast.BoolV(s)) ->
                            (match op with
                                "&&"  ->  Ast.BoolV(f && s)
                              | "||"  ->  Ast.BoolV(f || s)
                              | _   -> raise (Failure "undefined operation"))
                            | _   -> raise (Failure "impossible")

and evaluateFunc ctx l e arg = match l with
                                Ast.Single(x, _) -> evaluate ((x, arg)::ctx) e 
                              | Ast.Curry((x,_), xs) -> Ast.LambdaV ((x, arg)::ctx, xs, e)
