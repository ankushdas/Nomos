(* Stream Library *)
(* Imported from C0 compiler *)
(* Streams do not memoize *)

type 'a front =
    Cons of 'a * (unit -> 'a front)
  | Nil

and 'a stream = unit -> 'a front;;

let force ts = ts ()
