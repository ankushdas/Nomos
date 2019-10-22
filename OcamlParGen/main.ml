open Core
open Lexer
open Lexing

let rec print_args (args : Ast.arglist) = 
        match args with
                | Single(x,t) -> x
                | Curry((x,t), rest) -> let a : string = print_args(rest) in
                                    Printf.sprintf "%s %s"
                                    x
                                    a


let rec print_type (t : Ast.ocamlTP) = (match t with
                                        Integer -> "int"
                                     |  Boolean -> "bool"
                                     |  Arrow(t1, t2) -> Printf.sprintf "%s -> (%s)" (print_type t1) (print_type t2)
                                     |  ListTP(t1) -> Printf.sprintf "(%s) list" (print_type t1))

let rec print_list (l : Ast.expr list) = 
        match l with
        [] -> ""
     |  x::xs -> let a : string = print_ast(x)  in
                 let b : string = print_list(xs) in
                 if xs = [] then 
                 Printf.sprintf "%s" a
                 else
                 Printf.sprintf "%s,%s" a b



and print_ast (t : Ast.expr) = 
        match t with
           |  If(c1, c2, c3) ->
                           let a : string = print_ast(c1) in
                           let b : string = print_ast(c2) in
                           let c : string = print_ast(c3) in
                           Printf.sprintf "if (%s) then (%s) else (%s)"
                                                a
                                                b
                                                c
           | Var(x)  -> Printf.sprintf "%s" x
           | Bool(b) -> Printf.sprintf "%B" b
           | Int(i)  -> Printf.sprintf "%d" i
           | LetIn(Binding(x,e1,t), e2) -> let a : string = print_ast(e1) in
                                           let b : string = print_ast(e2) in
                                         Printf.sprintf "let (x = %s) in (%s)"
                                                        a
                                                        b
           | List(l) -> let a : string = print_list(l) in
                           Printf.sprintf "[%s]" a
           | Op(e1, opr, e2) -> let a : string = print_ast(e1) in
                                let b : string = print_ast(e2) in
                                Printf.sprintf "%s(%s, %s)"
                                                        opr
                                                        a
                                                        b
           
           | Ast.CompOp(e1, opr, e2) -> let a : string = print_ast(e1) in
                                    let b : string = print_ast(e2) in
                                    Printf.sprintf "%s(%s, %s)"
                                                        opr
                                                        a
                                                        b
           | Ast.RelOp(e1, opr, e2) -> let a : string = print_ast(e1) in
                                    let b : string = print_ast(e2) in
                                    Printf.sprintf "%s(%s, %s)"
                                                        opr
                                                        a
                                                        b


           | Cons(head, tail) -> let a : string = print_ast(head) in
                                 let b : string = print_ast(tail) in
                                 Printf.sprintf "(%s) :: (%s)"
                                                a
                                                b
           | Match((x,t),y,a,b,c) -> let i : string = print_ast(x) in
                                 let p : string = print_ast(y) in
                                 let q : string = print_ast(c) in
                                 Printf.sprintf "match (%s) with 
                                                 | [] -> (%s) 
                                                 | (%s) :: (%s) -> (%s)"
                                                 i p a b q
           | Lambda(args, body) -> let p : string = print_args(args) in
                                   let q : string = print_ast(body)  in
                                   Printf.sprintf "fun (%s) -> (%s)"
                                   p q
           | App ((expr1, t1), (expr2, t2)) -> let i : string = print_ast(expr1) in
                                   let j : string = print_ast(expr2) in
                                   Printf.sprintf "(%s)(%s)"
                                   i
                                   j
         
 
type context = (string * Ast.ocamlTP) list
exception TypeError of string

let format_err (e : Ast.expr) (t : Ast.ocamlTP) = 
        let a : string = print_ast(e) in
        let b : string = print_type(t) in
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


let rec typecheck (ctx : context) (e : Ast.expr) (t : Ast.ocamlTP) : bool = 
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



and contextToString ctx : string = (match ctx with
                                []  -> ""
                            |   (x,t1)::rest -> let v : string = (contextToString rest) in
                                            Printf.sprintf "(%s : %s), %s" x (print_type t1) v)




type valContext = (string * Ast.value) list


let rec print_list_val (l : Ast.value list) = 
        match l with
        [] -> ""
     |  x::xs -> let a : string = print_value(x)  in
                 let b : string = print_list_val(xs) in
                 if xs = [] then 
                 Printf.sprintf "%s" a
                 else
                 Printf.sprintf "%s,%s" a b
and print_value v = match v with 
                          Ast.IntV(v1) -> Printf.sprintf "%d" v1 
                        | Ast.BoolV(v1) -> Printf.sprintf "%b" v1
                        | Ast.ListV(l) -> "[" ^ print_list_val l ^ "]"
                        | Ast.LambdaV(_, args, v1) -> Printf.sprintf "fun %s -> %s" (print_args args) (print_ast v1)



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







let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  printf "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.file Lexer.token lexbuf with
  | SyntaxError msg ->
     (Printf.printf "LEXING FAILURE: %a: %s\n" print_position lexbuf msg; None)
  | Parser.Error ->
     (Printf.printf "PARSING FAILURE: %a\n" print_position lexbuf; None)


let rec process (l : Ast.program list) =
        match l with
                [] -> ()
        | Ast.Program(expr, t)::es -> try
                                      let a : bool = typecheck [] expr t in       
                                      (if a then let evalRes = evaluate [] expr in Printf.printf "%s\n" (print_value evalRes)
                                      else Printf.printf "TYPECHECKING FAILURE\n"; process es)
                                      with
                                      | TypeError err -> (Printf.printf "TYPECHECKING FAILURE: %s\n" err; process es)

(* part 1 *)
let rec parse_and_print lexbuf =
  match parse_with_error lexbuf with
  | Some (Ast.PL l) -> (process l; Printf.printf "DONE\n")
  | None -> ()

let () =
  let inx = In_channel.read_all "./test.ml" in
  let lexbuf = Lexing.from_string inx in
  parse_and_print lexbuf

