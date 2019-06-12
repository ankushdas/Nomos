(* Stream Library *)
(* Imported from C0 compiler *)
(* Streams do not memoize *)

type 'a front =
    Cons of 'a * (unit -> 'a front)
  | Nil

and 'a stream = unit -> 'a front;;

let force ts = ts ()

let rec fromList l () = match l with
    [] -> Nil
  | t :: ts -> Cons (t, fromList ts);;

let isNil s = match s with
    Nil -> true
  | Cons _ -> false;;
