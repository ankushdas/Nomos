module P = Print

type valContext = (string * Ast.value) list
exception EvaluationError of string

let rec lookupInContext (ctx : valContext) x = 
        match ctx with
                [] -> raise (EvaluationError "Empty context")
           | (y,v)::ys -> if x = y then v else lookupInContext ys x


let rec addValue (e : Ast.typed_expr) : Ast.valued_expr = 
        let valVar = Ast.IntV(0) in
        match e.structure with
                If(e1, e2, e3) -> {structure = If(addValue e1, addValue e2, addValue e3); data = valVar}
        |       LetIn(x, e1, e2) -> {structure = LetIn(x, addValue e1, addValue e2); data = valVar} 
        |       Bool(x)  -> {structure = Bool(x); data = valVar}
        |       Int(x)  -> {structure = Int(x); data = valVar}
        |       Var(x)   -> {structure = Var(x); data = valVar}
        |       List(l)  -> {structure = List(List.map addValue l); data = valVar} 
        |       App(l) -> {structure = App(List.map addValue l); data = valVar} 
                        
        |       Cons(x, xs) -> {structure = Cons(addValue x, addValue xs); data = valVar}
        |       Match(e1, e2, x, xs, e3) -> {structure = Match(addValue e1, addValue e2, x, xs, addValue e3); data = valVar} 
        |       Lambda(args, e) -> {structure = Lambda(args, addValue e); data = valVar} 
        |       Op(e1, o, e2) -> {structure = Op(addValue e1, o, addValue e2); data =  valVar}
        |       CompOp(e1, o, e2) -> {structure = CompOp(addValue e1, o, addValue e2); data =  valVar} 
        |       RelOp(e1, o, e2) -> {structure = RelOp(addValue e1, o, addValue e2); data = valVar} 
        |       Print(id, args) -> {structure = Print(id, List.map addValue args); data = valVar}




let rec evaluate (ctx : valContext) (e : Ast.valued_expr) : Ast.valued_expr =
        let value = 
        match e.structure with
        If(e1, e2, e3) -> (let ifVal = (evaluate ctx e1).data in
                          let thenVal = (evaluate ctx e2).data in
                          let elseVal = (evaluate ctx e3).data in
                          match ifVal with
                            Ast.BoolV(true) -> thenVal
                          | Ast.BoolV(false) -> elseVal
                          | _                -> raise (EvaluationError "Impossible"))

        | Bool (v) ->  Ast.BoolV (v)
        | Int (v)  ->  Ast.IntV(v)
        | Var(x) -> lookupInContext ctx x 
        | LetIn (var, expr, e') -> let letVal = (evaluate ctx expr).data in
                                                   let expVal = (evaluate ((var, letVal)::ctx) e').data in
                                                   expVal

        | List (l) -> (match l with
                         [] -> Ast.ListV([])
                     |   (x::xs) -> let firstVal = (evaluate ctx x).data in
                     let secondVal = (evaluate ctx {structure = Ast.List(xs); data = Ast.IntV(0)}).data in
                                      match secondVal with
                                          Ast.ListV(lst) -> Ast.ListV(firstVal::lst)
                                        | _              -> raise (EvaluationError "Impossible"))

        | App (l) -> (match l with
                        [] -> raise (EvaluationError "Impossible")
                |       [x] -> raise (EvaluationError "Impossible")
                |   e::es ->  let funcVal = (evaluate ctx e).data in evaluateApp funcVal es)

        | Cons (x, xs) -> (let firstVal = (evaluate ctx x).data in
                          let secondVal = (evaluate ctx xs).data in 
                                        match secondVal with
                                          Ast.ListV(lst) -> Ast.ListV(firstVal::lst)
                                        | _              -> raise (EvaluationError "Impossible"))
        | Match (e1, e2, id1, id2, e3) -> (let matchVal = (evaluate ctx e1).data in
                                               match matchVal with
                                                    Ast.ListV([]) -> (evaluate ctx e2).data
                                                | Ast.ListV(v::vs) -> (evaluate ((id1, v)::(id2, Ast.ListV(vs))::ctx) e3).data
                                                | _                -> raise (EvaluationError "Impossible"))


        | Lambda(l, e) -> Ast.LambdaV(ctx, l, e)
        | Op (e1, op, e2) -> (let firstArg = (evaluate ctx e1).data in
                            let secondArg = (evaluate ctx e2).data in
                            match (firstArg, secondArg) with
                              (Ast.IntV(f), Ast.IntV(s)) ->
                            (match op with
                                "+" -> Ast.IntV(f + s)
                              | "-" -> Ast.IntV(f - s)
                              | "/" -> Ast.IntV(f / s)
                              | "*" -> Ast.IntV(f * s)
                              | _   -> raise (EvaluationError "undefined operation"))
                            | _   -> raise (EvaluationError "impossible"))
        | Ast.CompOp (e1, op, e2) -> (let firstArg = (evaluate ctx e1).data in
                            let secondArg = (evaluate ctx e2).data in
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
        | Ast.RelOp (e1, op, e2) -> begin
                            let firstArg = (evaluate ctx e1).data in
                            let secondArg = (evaluate ctx e2).data in
                            match (firstArg, secondArg) with
                              (Ast.BoolV(f), Ast.BoolV(s)) ->
                            (match op with
                                "&&"  ->  Ast.BoolV(f && s)
                              | "||"  ->  Ast.BoolV(f || s)
                              | _   -> raise (EvaluationError "undefined operation"))
                            | _   -> raise (EvaluationError "impossible")
        end
        | Ast.Print (ids, args) -> match ids with
                                        [] -> Ast.IntV(0)
                                | id::ids' -> let cont = 
                                                  match id with
                                                Ast.Word(s) -> if (List.length ids' = 0)
                                                               then (Printf.printf "%s" s; args)
                                                               else (Printf.printf "%s " s; args)
                                        |    Ast.Newline(_) -> Printf.printf "\n"; args 
                                        |                 _ -> let value = evaluate ctx (List.hd args) in
                                                               if (List.length ids' = 0)
                                                               then (Printf.printf "%s" (P.print_value value.data); List.tl args)
                                                               else (Printf.printf "%s " (P.print_value value.data); List.tl args)  
                                                               in
                                              (evaluate ctx ({structure = Ast.Print(ids', cont); data = e.data})).data
        in
                {structure = e.structure; data = value}

and evaluateFunc ctx l e arg = match l with
                                Ast.Single(x) -> (evaluate ((x, arg)::ctx) e).data 
                              | Ast.Curry(x, xs) -> Ast.LambdaV ((x, arg)::ctx, xs, e)

and evaluateApp f l = 
        match (l, f) with
                ([], _) -> raise (EvaluationError "Impossible")
        |       ([e], Ast.LambdaV(ctx, l, e1)) -> let argVal = (evaluate ctx e).data in evaluateFunc ctx l e1 argVal
        |       (e::es, Ast.LambdaV(ctx, l, e1)) -> let argVal = (evaluate ctx e).data in evaluateApp (evaluateFunc ctx l e1 argVal) es
        |       _            -> raise (EvaluationError "Impossible")

