let rec print_args (args : Ast1.arglist) = 
  match args with
          | Single(x) -> x
          | Curry(x, rest) -> let a : string = print_args(rest) in
                              Printf.sprintf "%s %s"
                              x
                              a


let rec print_type (t : Ast1.ocamlTP) = (match t with
                                  Integer -> "int"
                               |  Boolean -> "bool"
                               |  Arrow(t1, t2) -> Printf.sprintf "%s -> (%s)" (print_type t1) (print_type t2)
                               |  ListTP(t1) -> Printf.sprintf "(%s) list" (print_type t1)
                               |  VarT(x) -> Printf.sprintf "%s" x)

let rec print_list (l : Ast1.expr list) = 
  match l with
  [] -> ""
|  x::xs -> let a : string = print_ast(x)  in
           let b : string = print_list(xs) in
           if xs = [] then 
           Printf.sprintf "%s" a
           else
           Printf.sprintf "%s,%s" a b

and print_app (l : Ast1.expr list) = 
  match l with
  [] -> ""
|  x::xs -> let a : string = print_ast(x)  in
           let b : string = print_list(xs) in
           if xs = [] then 
           Printf.sprintf "%s" a
           else
           Printf.sprintf "%s %s" a b

and print_ast (t : Ast1.expr) = 
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
     | LetIn(x, e1, e2) -> let a : string = print_ast(e1) in
                                     let b : string = print_ast(e2) in
                                   Printf.sprintf "let (%s = %s) in (%s)"
                                                  x
                                                  a
                                                  b
      | FreeLet(x, e1, e2) -> let a : string = print_ast(e1) in
                                     let b : string = print_ast(e2) in
                                   Printf.sprintf "freelet (%s = %s) in \n (%s)"
                                                  x
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
     
     | Ast1.CompOp(e1, opr, e2) -> let a : string = print_ast(e1) in
                              let b : string = print_ast(e2) in
                              Printf.sprintf "%s(%s, %s)"
                                                  opr
                                                  a
                                                  b
     | Ast1.RelOp(e1, opr, e2) -> let a : string = print_ast(e1) in
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
     | Match(x,y,a,b,c) -> let i : string = print_ast(x) in
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
     | App l -> let a : string = print_app(l) in
                     Printf.sprintf "%s" a
    


