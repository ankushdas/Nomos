type ocamlTP = 
        | Integer
        | Boolean
        | ListTP of ocamlTP
        | Arrow of ocamlTP * ocamlTP
        | Var of string



let rec print_type (t : ocamlTP) = (match t with
                                        Integer -> "int"
                                     |  Boolean -> "bool"
                                     |  Arrow(t1, t2) -> Printf.sprintf "%s -> (%s)" (print_type t1) (print_type t2)
                                     |  ListTP(t1) -> Printf.sprintf "(%s) list" (print_type t1)
                                     |  Var(x) -> Printf.sprintf "%s" x)

type substitution = (string * ocamlTP) list

let rec occurs (x : string) (t : ocamlTP) = 
        match t with
                Integer -> false
        |       Boolean -> false
        |       ListTP(t1) -> occurs x t1
        |       Arrow(t1, t2) -> occurs x t1 || occurs x t2
        |       Var(y) -> x = y


let rec subst (s : ocamlTP) (x : string) (t : ocamlTP) = 
        match t with
                Integer -> t
         |      Boolean -> t
         |      ListTP(t1) -> ListTP(subst s x t1)
         |      Arrow(t1, t2) -> Arrow(subst s x t1, subst s x t2)
         |      Var(y) -> if x = y then s else t

let apply (s : substitution) (t : ocamlTP) : ocamlTP = 
        List.fold_right (fun (x, u) -> subst u x) s t



let rec unify_one (s : ocamlTP) (t : ocamlTP) = 
        match (s,t) with
                (Integer, Integer) -> []
            |   (Boolean, Boolean) -> []
            |   (ListTP(x), ListTP(y)) -> unify [(x,y)]
            |   (Arrow(x1, y1), Arrow(x2, y2)) -> (unify [(x1, x2)]) @ (unify [(y1, y2)])
            |   (Var(x), Var(y)) -> if x = y then [] else [(x,t)]
            |   (Var(x), Integer) -> [(x, Integer)]
            |   (Integer, Var(x)) -> [(x, Integer)]
            |   (Var(x), Boolean) -> [(x, Boolean)]
            |   (Boolean, Var(x)) -> [(x, Boolean)]
            |   (Var(x), ListTP(y)) -> if occurs x y then raise (Failure "circularity") else [(x, t)]
            |   (ListTP(y), Var(x)) -> if occurs x y then raise (Failure "circularity") else [(x, s)]
            |   (Var(x), Arrow(y, z)) -> if occurs x t then raise (Failure "circularity") else [(x, t)]
            |   (Arrow(y,z), Var(x)) -> if occurs x s then raise (Failure "circularity") else [(x, s)]
            |   _                    -> raise (Failure "Impossible")
            

and unify (l : (ocamlTP * ocamlTP) list) : substitution = match l with
                                                                [] -> []
                                                       | (x,y)::t -> 
                                                                       let t1 = unify t in
                                                                       let t2 = unify_one (apply t1 x) (apply t1 y) in
                                                                       t2 @ t1


let rec print_sub l = match l with
                        [] -> ""
                 | (s, t)::xs -> match xs with
                                   |    _  -> Printf.sprintf "(%s = %s), %s" s (print_type t) (print_sub xs)

let () =
        let a = unify [(Arrow(Integer, Arrow(Integer, Integer)), Arrow(Var("a"), Var("b")))] in
        let _ = Printf.printf "[%s]\n" (print_sub a) in
        ()
