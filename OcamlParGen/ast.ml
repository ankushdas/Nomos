type ocamlTP = 
        | Integer
        | Boolean
        | ListTP of ocamlTP
        | Arrow of ocamlTP * ocamlTP

type arglist = Single of string * ocamlTP 
             | Curry of (string * ocamlTP) * arglist

type  expr = 
        | If of expr * expr * expr
        | LetIn of binding * expr
        | Bool of bool
        | Int of int
        | Var of string
        | List of expr list
        | App of (expr * ocamlTP) * (expr * ocamlTP)
        | Cons of expr * expr
        | Match of (expr * ocamlTP) * expr * string * string * expr
        | Lambda of arglist * expr
        | Op of expr * string * expr
and binding = Binding of (string * expr * ocamlTP)
type program = Program of expr * ocamlTP 
