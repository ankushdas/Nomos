5 : int;;
if true then false else true : bool;;
if true then 5 else 6 : bool;;
app((fun (m : int) -> m + 1) : int -> int, 5 : int) : int;;
let x : int = 5 in x : int;;
let x : int list = [5,6,7] in match x : int list with [] -> [5] | y::ys -> ys : int list;;
app(app((fun (m : int) (n : int) -> m + n) : int -> int -> int, 5 : int) : int -> int, 4 : int) : int;;
app(if true then (fun (m : int) -> m) else (fun (x : int) -> x + 1) : int -> int, 7 : int) : int;;
if ((1 + 2 = 5) || (5*7 <> 35)) then 7*8 else 6/3 : int;;
(fun (m : bool) (n : bool) (o : bool) -> m + n = o) : bool;;
[5]::[] : int
