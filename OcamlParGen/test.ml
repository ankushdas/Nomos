5;;
if true then false else false;;
[];;
5::[6];;
fun m -> m+1;;
let f = fun x -> x::[5] in f;;
let x = [5,6] in match x with [] -> true | y::ys -> false;;
(fun m n o -> m + n - o) 5 6 7;;
(if (5 + 6 > 9 && 4 / 2 = 2) then fun x u z -> x else fun a b c -> (match a with [] -> [] | y::ys -> ys));;
(fun a b c -> match a with [] -> [] | y::ys -> ys);;
[]::[]::[];;
let fun f x = 1 in f;;
if (fun p -> match p with [] -> true | x::xs -> false) ([]) then 5 else 6
