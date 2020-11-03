type ocamlTP = 
        | Integer
        | Boolean
        | ListTP of ocamlTP
        | Arrow of ocamlTP * ocamlTP
        | VarT of string

type arglist = Single of string 
             | Curry of string * arglist

type print_id =   Word of string 
                | Int of unit 
                | Bool of unit
                | Newline of unit

type 'a aug_expr = 
        {
                structure : 'a expr;
                data      : 'a;
        }
and 'a expr = 
        | If of 'a aug_expr * 'a aug_expr * 'a aug_expr
        | LetIn of string * 'a aug_expr * 'a aug_expr
        | Bool of bool
        | Int of int
        | Var of string
        | List of 'a aug_expr list
        | App of 'a aug_expr list
        | Cons of 'a aug_expr * 'a aug_expr
        | Match of 'a aug_expr * 'a aug_expr * string * string * 'a aug_expr
        | Lambda of arglist * 'a aug_expr
        | Op of 'a aug_expr * string * 'a aug_expr
        | CompOp of 'a aug_expr * string * 'a aug_expr
        | RelOp of 'a aug_expr * string * 'a aug_expr
        | Print of print_id list * 'a aug_expr list

type untyped_expr = unit aug_expr
type typed_expr = ocamlTP aug_expr

type value = IntV of int
          | BoolV of bool
          | ListV of value list
          | LambdaV of context * arglist * valued_expr
and context = (string * value) list
and valued_expr = value aug_expr

type program = Program of untyped_expr
type programList = PL of program list
