let rec print_args (args : Ast.arglist) = 
        match args with
                | Single(x) -> x
                | Curry(x, rest) -> let a : string = print_args(rest) in
                                    Printf.sprintf "%s %s"
                                    x
                                    a


let rec print_type (t : Ast.ocamlTP) = (match t with
                                        Integer -> "int"
                                     |  Boolean -> "bool"
                                     |  Arrow(t1, t2) -> Printf.sprintf "%s -> (%s)" (print_type t1) (print_type t2)
                                     |  ListTP(t1) -> Printf.sprintf "(%s) list" (print_type t1)
                                     |  VarT(x) -> Printf.sprintf "%s" x)

let rec print_list (l : 'a Ast.aug_expr list) = 
        match l with
        [] -> ""
     |  x::xs -> let a : string = print_ast(x.structure)  in
                 let b : string = print_list(xs) in
                 if xs = [] then 
                 Printf.sprintf "%s" a
                 else
                 Printf.sprintf "%s,%s" a b



and print_ast (t : 'a Ast.expr) = 
        match t with
           |  If(c1, c2, c3) ->
                           let a : string = print_ast(c1.structure) in
                           let b : string = print_ast(c2.structure) in
                           let c : string = print_ast(c3.structure) in
                           Printf.sprintf "if (%s) then (%s) else (%s)"
                                                a
                                                b
                                                c
           | Var(x)  -> Printf.sprintf "%s" x
           | Bool(b) -> Printf.sprintf "%B" b
           | Int(i)  -> Printf.sprintf "%d" i
           | LetIn(x, e1, e2) -> let a : string = print_ast(e1.structure) in
                                           let b : string = print_ast(e2.structure) in
                                         Printf.sprintf "let (x = %s) in (%s)"
                                                        a
                                                        b
           | List(l) -> let a : string = print_list(l) in
                           Printf.sprintf "[%s]" a
           | Op(e1, opr, e2) -> let a : string = print_ast(e1.structure) in
                                let b : string = print_ast(e2.structure) in
                                Printf.sprintf "%s(%s, %s)"
                                                        opr
                                                        a
                                                        b
           
           | Ast.CompOp(e1, opr, e2) -> let a : string = print_ast(e1.structure) in
                                    let b : string = print_ast(e2.structure) in
                                    Printf.sprintf "%s(%s, %s)"
                                                        opr
                                                        a
                                                        b
           | Ast.RelOp(e1, opr, e2) -> let a : string = print_ast(e1.structure) in
                                    let b : string = print_ast(e2.structure) in
                                    Printf.sprintf "%s(%s, %s)"
                                                        opr
                                                        a
                                                        b


           | Cons(head, tail) -> let a : string = print_ast(head.structure) in
                                 let b : string = print_ast(tail.structure) in
                                 Printf.sprintf "(%s) :: (%s)"
                                                a
                                                b
           | Match(x,y,a,b,c) -> let i : string = print_ast(x.structure) in
                                 let p : string = print_ast(y.structure) in
                                 let q : string = print_ast(c.structure) in
                                 Printf.sprintf "match (%s) with 
                                                 | [] -> (%s) 
                                                 | (%s) :: (%s) -> (%s)"
                                                 i p a b q
           | Lambda(args, body) -> let p : string = print_args(args) in
                                   let q : string = print_ast(body.structure)  in
                                   Printf.sprintf "fun (%s) -> (%s)"
                                   p q
           | App l -> let a : string = print_list(l) in
                           Printf.sprintf "[%s]" a
           | Print (l, arg) -> let s1 = List.map (fun x -> match x with
                                                                Ast.Word(s) -> Printf.sprintf "%s " s 
                                                           |       Ast.Int(_) -> Printf.sprintf "int "
                                                           |      Ast.Bool(_) -> Printf.sprintf "bool "
                                                           |       Ast.Newline(_) -> Printf.sprintf "newline ") l in
                                 let s1'  = List.fold_left (fun x y -> x^y) "" s1 in
                                 let s2 = print_list(arg) in
                                 Printf.sprintf "%s %s" s1' s2





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
                        | Ast.LambdaV(_, args, v1) -> Printf.sprintf "fun %s -> %s" (print_args args) (print_ast v1.structure)



let rec print_constraints (l : (Ast.ocamlTP * Ast.ocamlTP) list) = 
                        match l with
                                [] -> ""
                        | (t1, t2)::xs -> Printf.sprintf "(%s, %s) \n %s"
                                        (print_type(t1))
                                        (print_type(t2))
                                        (print_constraints xs)

let rec print_sub l = match l with
                        [] -> ""
                 | (s, t)::xs -> match xs with
                                   |    _  -> Printf.sprintf "(%s = %s), %s" s (print_type t) (print_sub xs)


