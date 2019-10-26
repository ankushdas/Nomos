module P = Print

type context = (string * Ast.ocamlTP) list
exception TypeError of string

let format_err (e : Ast.expr) (t : Ast.ocamlTP) = 
        let a : string = P.print_ast(e) in
        let b : string = P.print_type(t) in
          Printf.sprintf "expression %s did not have type %s" a b

let rec get_result_type (t : Ast.ocamlTP) = 
        match t with
                Ast.Arrow(t1, t2) -> get_result_type(t2)
         |      _                 -> t


let rec type_equals (t1 : Ast.ocamlTP) (t2 : Ast.ocamlTP) = 
        match (t1, t2) with
                (Ast.Integer, Ast.Integer) -> true
        |       (Ast.Boolean, Ast.Boolean) -> true
        |       (Ast.Arrow(x, y), Ast.Arrow(a, b)) -> (type_equals x a) && (type_equals y b)
        |       (Ast.ListTP(a), Ast.ListTP(b)) -> type_equals a b
        |       _                            -> false

let rec getType (ctx : context) (x : string) =
        match ctx with
                [] -> raise (TypeError (Printf.sprintf "Unbound variable %s" x))
           |    (y, tp)::xs -> if x = y then tp else getType xs x


(*let rec typecheck (ctx : context) (e : Ast.expr) (t : Ast.ocamlTP) : bool = 
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
        | App (l) -> (match l with
                        [] -> raise (TypeError "Impossible")
                 |     [x] -> raise (TypeError "Impossible")
            | (e1, t1)::es -> let t2 = get_peeled_type es t1 in type_equals t2 t) 
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

and checkArglist (l : Ast.arglist) (t : Ast.ocamlTP) = 
        match l with
                Ast.Single(_, t1) -> t1 = t
            |   Ast.Curry ((_, t1), rest) -> match t with
                                                Arrow(t2, t3) -> if (type_equals t1 t2) && (checkArglist rest t3)
                                                then true
                                                else false
                                                |  _  -> false 
and addArglist l ctx = match l with
                                Ast.Single(x, t1) -> (1, (x,t1)::ctx)
                            |   Ast.Curry((x,t1), rest) -> 
                                            let
                                                (len, ctx') = addArglist rest ctx
                                            in
                                                (len + 1, (x,t1)::ctx')

and get_peeled_type l t = match (l, t) with
                                ([], _) -> raise (TypeError "Impossible")
         |  ([(e1, t1)], Arrow(t2, t3)) -> if (type_equals t1 t2) then t3 else raise (TypeError "app rule failed")
         |  ((e1, t1)::es, Arrow(t2, t3)) -> if (type_equals t1 t2) then get_peeled_type es t3 else raise (TypeError "app rule failed")
         |                          _    -> raise (TypeError "Impossible")
         *)
