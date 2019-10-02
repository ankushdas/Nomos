let rec print_args (args : Ast.arglist) = 
        match args with
                | Single(x) -> x
                | Curry(x, rest) -> let a : string = print_args(rest) in
                                    Printf.sprintf "%s %s"
                                    x
                                    a

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
           |  IfWithElse(c1, c2, c3) ->
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
           | LetIn(Binding(x,e1), e2) -> let a : string = print_ast(e1) in
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
           | App (expr1, expr2) -> let i : string = print_ast(expr1) in
                                   let j : string = print_ast(expr2) in
                                   Printf.sprintf "(%s)(%s)"
                                   i
                                   j
 


let process (line : string) =
  let linebuf = Lexing.from_string line in
  try
  let res = Parser.prog Lexer.token linebuf
  in
  match res with
        None -> Printf.printf "%s" "None"
      | Some (v) ->   let a : string = print_ast(v) in
                      Printf.printf "%s" a
  with
  | Lexer.SyntaxError msg ->
      Printf.fprintf stderr "%s%!" msg
  | Parser.Error ->
      Printf.fprintf stderr "At offset %d: syntax error.\n%!" (Lexing.lexeme_start linebuf)

let process (optional_line : string option) =
  match optional_line with
  | None ->
      ()
  | Some line ->
      process line

let rec repeat channel =
  (* Attempt to read one line. *)
  let optional_line, continue = Lexer.line channel in
  process optional_line;
  if continue then
    repeat channel
  
let () =
  repeat (Lexing.from_channel stdin)
