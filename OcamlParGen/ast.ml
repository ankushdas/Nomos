type ocamlTP = 
        | Integer
        | Boolean
        | ListTP of ocamlTP
        | Arrow of ocamlTP * ocamlTP
        | VarT of string

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
        | CompOp of expr * string * expr
        | RelOp of expr * string * expr
(*and typedExpr = 
        | IfT of typedExpr * typedExpr * typedExpr * ocamlTP
        | LetInT of bindingT * typedExpr * ocamlTP
        | BoolT of bool * ocamlTP
        | IntT of int * ocamlTP
        | VarT of string * ocamlTP
        | ListT of expr list * ocamlTP
        | AppT of typedExpr * typedExpr * ocamlTP
        | ConsT of typedExpr * typedExpr * ocamlTP
        | MatchT of typedExpr * typedExpr * string * string * typedExpr * ocamlTP
        | LambdaT of arglist * typedExpr * ocamlTP
        | OpT of typedExpr * string * typedExpr * ocamlTP
        | CompOpT of typedExpr * string * typedExpr * ocamlTP
        | RelOpT of typedExpr * string * typedExpr * ocamlTP*)
and binding = Binding of (string * expr * ocamlTP)
(*and bindingT = BindingT of (string * typedExpr)*)

type program = Program of expr * ocamlTP



type value = IntV of int
          | BoolV of bool
          | ListV of value list
          | LambdaV of context * arglist * expr
and context = (string * value) list

type programList = PL of program list
