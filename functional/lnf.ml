module A = Ast1

let var = ref 0

let fresh () : string = 
        let _ = var := !var + 1 in
        let res : int = !var in 
        let a : string = Printf.sprintf "x%d" res in a


let isValue e = match e with
  A.Bool(_) -> true
| A.Int(_)  -> true
| A.Var(_)  -> true
| _           -> false


let rec consify l = match l with
    [] -> A.List([])
  | e::es ->
      A.Cons(e, consify es);;


let id x = x
let rec normalize (e : A.expr) (k :  A.expr -> A.expr) =
  match e with
    A.Bool(b) -> 
    k(e)
  | A.Int(n) -> k(e)
  | A.Var(_) -> k(e)
  | A.List(l) -> k e
  | A.If(e1, e2, e3) -> 
  begin
  normalize e1 (fun res1 -> 
                let v1 = fresh () in
                k(A.FreeLet(v1, res1, 
                    A.If(A.Var(v1), e2, e3))))
  end
  (* not sure what to do in this case *)
  | A.LetIn(x, e1, e2) -> k e
  | A.Cons(x, xs) -> 
  begin
                    normalize x (fun res1 ->
                    let v1 = fresh () in
                    normalize xs (fun res2 -> 
                    let v2 = fresh () in
                    A.FreeLet(v1, res1, A.FreeLet(v2, res2, k(A.Cons(A.Var(v1), A.Var(v2)))))))
  end
  | A.Match(e1, e2, x, xs, e3) ->
  begin
              normalize e1 (fun res1 ->
              let v1 = fresh () in
              k(A.FreeLet(v1, res1, 
              A.Match(A.Var(v1), e2, x, xs, e3))))
  end
  | A.App(l) -> 
  begin
  match l with
                [] -> raise (Failure "Impossible")
              | [x] -> normalize x k
              | x::y::xs -> normalize x (
                fun res1 -> let v1 = fresh () in  
                normalize y (
                fun res2 -> let v2  = fresh () in
                let v3 = fresh () in
                A.FreeLet(v1, res1, A.FreeLet(
                  v2, res2, 
                  A.FreeLet(v3, A.App[A.Var(v1); A.Var(v2)], 
                  normalize 
                  (A.App(A.Var(v3)::xs)) k)))))
  end
  | A.Lambda(args, body) -> k(e)
  | A.Op(e1, o, e2) -> normalize e1 (fun res1 -> let v1 = fresh () in
                       normalize e2 (fun res2 -> let v2 = fresh () in 
                       k(A.FreeLet(v1, res1, A.FreeLet(v2, res2, A.Op(A.Var(v1), o, A.Var(v2)))))))
  | A.CompOp(e1, o, e2) -> normalize e1 (fun res1 -> let v1 = fresh () in
  normalize e2 (fun res2 -> let v2 = fresh () in 
  k(A.FreeLet(v1, res1, A.FreeLet(v2, res2, A.CompOp(A.Var(v1), o, A.Var(v2)))))))                     
  | A.RelOp(e1, o, e2) -> 
  begin 
    normalize e1 (fun res1 -> let v1 = fresh () in
  normalize e2 (fun res2 -> let v2 = fresh () in 
  k(A.FreeLet(v1, res1, A.FreeLet(v2, res2, A.RelOp(A.Var(v1), o, A.Var(v2))))))) 
  end 
  | _ -> raise (Failure "Impossible")
and normalize_term (e : A.expr) = normalize e (fun x -> x)


let main () = 
  (*let e = A.If(A.CompOp(A.Int(5), "=", A.Int(6)), 
  A.App([A.Var("f"); A.Int(4)]), A.Lambda(A.Single("x"), A.Bool(true))) in *)
  (* let e = A.Op(A.Int(5), "+", A.Op(A.Int(6), "-", A.Int(7))) in *)
  (* let e = A.LetIn("f", A.Lambda(A.Single("x"), A.Int(5)), A.Var("f")) in  *)
  (* let e = A.App([A.Var("f"); A.Var("g"); A.Var("h"); A.Var("i")]) in *)
  (* let e = A.LetIn("x", A.If(A.Int(5), A.Int(6), A.Int(7)), A.If(A.Var("x"), A.Bool(true), A.Bool(false))) in *)
  (* let e = A.Cons(A.Cons(A.Var("x"), A.Var("xs")), A.List([])) in *)
  (* let e =  *)
    (* A.Match(A.Int(5), A.Int(4), "x", "xs", A.Int(3)) in *)
  let e = A.LetIn("x", A.Int(5), A.Var("x")) in
  Printf.printf "%s\n" (Print1.print_ast (normalize e (fun x -> x)));;
main ()