type valContext = (string * Ast.value) list
exception EvaluationError of string

let rec lookupInContext (ctx : valContext) x = 
        match ctx with
                [] -> raise (EvaluationError "Empty context")
           | (y,v)::ys -> if x = y then v else lookupInContext ys x





let rec evaluate (ctx : valContext) (e : Ast.expr) : Ast.value = 
        match e with
        If(e1, e2, e3) -> (let ifVal = evaluate ctx e1 in
                          let thenVal = evaluate ctx e2 in
                          let elseVal = evaluate ctx e3 in
                          match ifVal with
                            Ast.BoolV(true) -> thenVal
                          | Ast.BoolV(false) -> elseVal
                          | _                -> raise (EvaluationError "Impossible"))

        | Bool (v) ->  Ast.BoolV (v)
        | Int (v)  ->  Ast.IntV(v)
        | Var(x) -> lookupInContext ctx x 
        
        | LetIn (var, expr, e') -> let letVal = evaluate ctx expr in
                                                   let expVal = evaluate ((var, letVal)::ctx) e' in
                                                   expVal

        | List (l) -> (match l with
                         [] -> Ast.ListV([])
                     |   (x::xs) -> let firstVal = evaluate ctx x in
                                    let secondVal = evaluate ctx (Ast.List(xs)) in
                                      match secondVal with
                                          Ast.ListV(lst) -> Ast.ListV(firstVal::lst)
                                        | _              -> raise (EvaluationError "Impossible"))

        | App (l) -> (match l with
                        [] -> raise (EvaluationError "Impossible")
                |       [x] -> raise (EvaluationError "Impossible")
                |   e::es ->  let funcVal = evaluate ctx e in evaluateApp funcVal es)

        | Cons (x, xs) -> (let firstVal = evaluate ctx x in
                          let secondVal = evaluate ctx xs in 
                                        match secondVal with
                                          Ast.ListV(lst) -> Ast.ListV(firstVal::lst)
                                        | _              -> raise (EvaluationError "Impossible"))




        | Match (e1, e2, id1, id2, e3) -> (let matchVal = evaluate ctx e1 in
                                               match matchVal with
                                                    Ast.ListV([]) -> evaluate ctx e2
                                                | Ast.ListV(v::vs) -> evaluate ((id1, v)::(id2, Ast.ListV(vs))::ctx) e3
                                                | _                -> raise (EvaluationError "Impossible"))


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
                              | _   -> raise (EvaluationError "undefined operation"))
                            | _   -> raise (EvaluationError "impossible"))
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
                              | _   -> raise (EvaluationError "undefined operation"))
                            | _   -> raise (EvaluationError "impossible"))
        | Ast.RelOp (e1, op, e2) -> let firstArg = evaluate ctx e1 in
                            let secondArg = evaluate ctx e2 in
                            match (firstArg, secondArg) with
                              (Ast.BoolV(f), Ast.BoolV(s)) ->
                            (match op with
                                "&&"  ->  Ast.BoolV(f && s)
                              | "||"  ->  Ast.BoolV(f || s)
                              | _   -> raise (EvaluationError "undefined operation"))
                            | _   -> raise (EvaluationError "impossible")

and evaluateFunc ctx l e arg = match l with
                                Ast.Single(x) -> evaluate ((x, arg)::ctx) e 
                              | Ast.Curry(x, xs) -> Ast.LambdaV ((x, arg)::ctx, xs, e)

and evaluateApp f l = 
        match (l, f) with
                ([], _) -> raise (EvaluationError "Impossible")
        |       ([e], Ast.LambdaV(ctx, l, e1)) -> let argVal = evaluate ctx e in evaluateFunc ctx l e1 argVal
        |       (e::es, Ast.LambdaV(ctx, l, e1)) -> let argVal = evaluate ctx e in evaluateApp (evaluateFunc ctx l e1 argVal) es
        |       _            -> raise (EvaluationError "Impossible")

