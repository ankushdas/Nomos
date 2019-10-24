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
                                     |  ListTP(t1) -> Printf.sprintf "(%s) list" (print_type t1)
                                     |  Var(x) -> Printf.sprintf "%s" x)

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



let rec print_constraints (l : (Ast.ocamlTP * Ast.ocamlTP) list) = 
                        match l with
                                [] -> ""
                        | (t1, t2)::xs -> Printf.sprintf "(%s, %s) \n %s"
                                        (print_type(t1))
                                        (print_type(t2))
                                        (print_constraints(xs))
