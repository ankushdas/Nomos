type ocamlTP = 
        | Integer
        | Boolean
        | ListTP of ocamlTP
        | Arrow of ocamlTP * ocamlTP
        | VarT of string

type arglist = Single of string 
             | Curry of string * arglist

type  expr = 
        | If of expr * expr * expr
        | LetIn of string * expr * expr
        | Bool of bool
        | Int of int
        | Var of string
        | List of expr list
        | App of expr list
        | Cons of expr * expr
        | Match of expr * expr * string * string * expr
        | Lambda of arglist * expr
        | Op of expr * string * expr
        | CompOp of expr * string * expr
        | RelOp of expr * string * expr

type program = Program of expr



type value = IntV of int
          | BoolV of bool
          | ListV of value list
          | LambdaV of context * arglist * expr
and context = (string * value) list

type programList = PL of program list
