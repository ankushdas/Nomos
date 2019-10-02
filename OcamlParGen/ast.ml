type arglist = Single of string | Curry of string * arglist
type  expr = 
        | IfWithElse of expr * expr * expr
        | LetIn of binding * expr
        | Bool of bool
        | Int of int
        | Var of string
        | List of expr list
        | App of expr * expr
        | Cons of expr * expr
        | Match of expr * expr * string * string * expr
        | Lambda of arglist * expr
        | Op of expr * string * expr
and binding = Binding of (string * expr)
