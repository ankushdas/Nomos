if true then false else true : bool;;
if true then 5 else 6 : bool;;
app((fun (m : int) -> m + 1) : int -> int, 5 : int) : int;;
let x : int = 5 in x : int;;
let x : int list = [5,6,7] in match x : int list with [] -> true | y::ys -> false : bool
