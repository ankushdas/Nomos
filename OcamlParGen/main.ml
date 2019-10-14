(*let file = "test.txt"



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
                                                         let ctx' = addArglist l ctx in
                                                         if (typecheck ctx' e (get_result_type t))
                                                         then true
                                                         else raise (TypeError (format_err e t))
                                        | _        -> raise (TypeError (format_err e t)))
        | Op (e1, _, e2) -> let a : bool = typecheck ctx e1 t in
                            let b : bool = typecheck ctx e2 t in
                            if a && b && (type_equals t Ast.Integer)
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
                                Ast.Single(x, t1) -> (x,t1)::ctx
                            |   Ast.Curry((x,t1), rest) -> (x,t1)::(addArglist rest ctx)



and contextToString ctx : string = (match ctx with
                                []  -> ""
                            |   (x,t1)::rest -> let v : string = (contextToString rest) in
                                            Printf.sprintf "(%s : %s), %s" x (print_type t1) v)

let process (line : string) =
  let linebuf = Lexing.from_string line in
  try
  let Program(res,typ) = Parser.prog Lexer.token linebuf in
  let a : string = print_ast(res) in
  let _    = typecheck [] res typ in
        Printf.printf "The expression is: %s \nTypechecking succeded!\n" a
  with
  | TypeError err -> Printf.printf "Type error: %s" err
  | Lexer.SyntaxError msg ->
      Printf.fprintf stderr "%s%!\n" msg
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
        (*
  repeat (Lexing.from_channel stdin)
  *)
    let oc = open_in file in
*)
open Core
open Lexer
open Lexing


let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  printf "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.prog Lexer.token lexbuf with
  | SyntaxError msg ->
     (fprintf stderr "%a: %s %s\n" print_position lexbuf msg; None)
  | Parser.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)

(* part 1 *)
let rec parse_and_print lexbuf =
  match parse_with_error lexbuf with
  | Some value -> printf "hi"
  | None -> ()

let () =
  let inx = In_channel.read_all "./test.ml" in
  let _ = Printf.printf "%s" inx in
  let lexbuf = Lexing.from_string inx in
  parse_and_print lexbuf

