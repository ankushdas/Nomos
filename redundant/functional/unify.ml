module T = Typecheck
module A = Ast
module P = Print


type substitution = (string * Ast.ocamlTP) list



let rec occurs (x : string) (t : Ast.ocamlTP) = 
        match t with
                Integer -> false
        |       Boolean -> false
        |       ListTP(t1) -> occurs x t1
        |       Arrow(t1, t2) -> occurs x t1 || occurs x t2
        |       VarT(y) -> x = y

let rec subst (s : Ast.ocamlTP) (x : string) (t : Ast.ocamlTP) = 
        match t with
                Integer -> t
         |      Boolean -> t
         |      ListTP(t1) -> ListTP(subst s x t1)
         |      Arrow(t1, t2) -> Arrow(subst s x t1, subst s x t2)
         |      VarT(y) -> if x = y then s else t

let apply (s : substitution) (t : Ast.ocamlTP) : Ast.ocamlTP = 
        List.fold_right (fun (x, u) -> subst u x) s t




let rec unify_one (s : Ast.ocamlTP) (t : Ast.ocamlTP) = 
        let _ = P.print_type s in 
        let _ = P.print_type t in 
        match (s,t) with
                (Integer, Integer) -> []
            |   (Boolean, Boolean) -> []
            |   (ListTP(x), ListTP(y)) -> unify [(x,y)]
            |   (Arrow(x1, y1), Arrow(x2, y2)) -> (unify [(x1, x2)]) @ (unify [(y1, y2)])
            |   (VarT(x), VarT(y)) -> if x = y then [] else [(x, t)]
            |   (VarT(x), Integer) -> [(x, Integer)]
            |   (Integer, VarT(x)) -> [(x, Integer)]
            |   (VarT(x), Boolean) -> [(x, Boolean)]
            |   (Boolean, VarT(x)) -> [(x, Boolean)]
            |   (VarT(x), ListTP(y)) -> if occurs x y then raise (T.TypeError "circularity") else [(x, t)]
            |   (ListTP(y), VarT(x)) -> if occurs x y then raise (T.TypeError "circularity") else [(x, s)]
            |   (VarT(x), Arrow(y, z)) -> if occurs x t then raise (T.TypeError "circularity") else [(x, t)]
            |   (Arrow(y,z), VarT(x)) -> if occurs x s then raise (T.TypeError "circularity") else [(x, s)]
            |   _                    -> raise (T.TypeError "Impossible")
            

and unify (l : (Ast.ocamlTP * Ast.ocamlTP) list) : substitution = match l with
                                                                [] -> []
                                                       | (x,y)::t -> 
                                                                       let t1 = unify t in
                                                                       let t2 = unify_one (apply t1 x) (apply t1 y) in
                                                                       t2 @ t1


let rec find_type (s : substitution) = 
        match s with
                [] -> raise (T.TypeError "Impossible")
       | (x,t)::xs -> if x = "v1" then t else find_type xs



let rec find_res (s : substitution) = 
        match s with
                [] -> []
       | (x,t)::xs -> if x = "v1" then find_res xs else (x,t)::(find_res xs)

