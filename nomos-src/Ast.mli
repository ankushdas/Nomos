module R = Arith
type ext = Mark.ext option
type potential = Arith of R.arith | Star
type label = string
type tpname = string
type expname = string
type func_tp =
    Integer
  | Boolean
  | ListTP of func_tp * potential
  | Arrow of func_tp * func_tp
  | VarT of string
type mode = Shared | Linear | Transaction | Pure | Unknown | MVar of string
type str = Hash | Dollar
type chan = str * string * mode
type stype =
    Plus of choices
  | With of choices
  | Tensor of stype * stype * mode
  | Lolli of stype * stype * mode
  | One
  | PayPot of potential * stype
  | GetPot of potential * stype
  | TpName of tpname
  | Up of stype
  | Down of stype
  | FArrow of func_tp * stype
  | FProduct of func_tp * stype
and choices = (label * stype) list
type arglist = Single of string * ext | Curry of (string * ext) * arglist
type arith_operator = Add | Sub | Mult | Div
type comp_operator = Eq | Neq | Lt | Gt | Leq | Geq
type rel_operator = And | Or
type arg = STArg of chan | FArg of string
type 'a func_aug_expr = { func_structure : 'a func_expr; func_data : 'a; }
and 'a st_aug_expr = { st_structure : 'a st_expr; st_data : 'a; }
and 'a func_expr =
    If of 'a func_aug_expr * 'a func_aug_expr * 'a func_aug_expr
  | LetIn of string * 'a func_aug_expr * 'a func_aug_expr
  | Bool of bool
  | Int of int
  | Var of string
  | ListE of 'a func_aug_expr list
  | App of 'a func_aug_expr list
  | Cons of 'a func_aug_expr * 'a func_aug_expr
  | Match of 'a func_aug_expr * 'a func_aug_expr * string * string *
      'a func_aug_expr
  | Lambda of arglist * 'a func_aug_expr
  | Op of 'a func_aug_expr * arith_operator * 'a func_aug_expr
  | CompOp of 'a func_aug_expr * comp_operator * 'a func_aug_expr
  | RelOp of 'a func_aug_expr * rel_operator * 'a func_aug_expr
  | Tick of potential * 'a func_aug_expr
  | Command of 'a st_aug_expr
and 'a st_expr =
    Fwd of chan * chan
  | Spawn of chan * expname * arg list * 'a st_aug_expr
  | ExpName of chan * expname * arg list
  | Lab of chan * label * 'a st_aug_expr
  | Case of chan * 'a branches
  | Send of chan * chan * 'a st_aug_expr
  | Recv of chan * chan * 'a st_aug_expr
  | Close of chan
  | Wait of chan * 'a st_aug_expr
  | Work of potential * 'a st_aug_expr
  | Pay of chan * potential * 'a st_aug_expr
  | Get of chan * potential * 'a st_aug_expr
  | Acquire of chan * chan * 'a st_aug_expr
  | Accept of chan * chan * 'a st_aug_expr
  | Release of chan * chan * 'a st_aug_expr
  | Detach of chan * chan * 'a st_aug_expr
  | RecvF of chan * string * 'a st_aug_expr
  | SendF of chan * 'a func_aug_expr * 'a st_aug_expr
  | Let of string * 'a func_aug_expr * 'a st_aug_expr
  | IfS of 'a func_aug_expr * 'a st_aug_expr * 'a st_aug_expr
and 'a branch = label * 'a st_aug_expr
and 'a branches = 'a branch list
type parsed_expr = ext func_aug_expr
type typed_expr = func_tp func_aug_expr
type argument = Functional of string * func_tp | STyped of chan * stype
type chan_tp = chan * stype
type context = {
  shared : chan_tp list;
  linear : chan_tp list;
  ordered : argument list;
}
type decl =
    TpDef of tpname * stype
  | ExpDecDef of expname * mode * (context * potential * chan_tp) *
      parsed_expr
  | Exec of expname
type program = (decl * ext) list * ext
type value =
    IntV of int
  | BoolV of bool
  | ListV of value list
  | LambdaV of value_context * arglist * valued_expr
and value_context = (string * value) list
and valued_expr = value func_aug_expr
type msg =
    MLabI of chan * label * chan
  | MLabE of chan * label * chan
  | MSendT of chan * chan * chan
  | MSendL of chan * chan * chan
  | MClose of chan
  | MPayP of chan * potential * chan
  | MPayG of chan * potential * chan
  | MSendP of chan * valued_expr * chan
  | MSendA of chan * valued_expr * chan
exception AstImpossible
exception UndeclaredTp
val lookup_tp : (decl * 'a) list -> tpname -> stype option
val expd_tp : (decl * 'a) list -> tpname -> stype
val lookup_expdec :
  (decl * 'a) list ->
  expname -> (context * potential * chan_tp * mode) option
val lookup_expdef : (decl * 'a) list -> expname -> parsed_expr option
val lookup_choice : ('a * 'b) list -> 'a -> 'b option
val is_shared : (decl * 'a) list -> stype -> bool
val sub : 'a * 'b * 'c -> 'd * 'b * 'e -> 'a * 'b * 'c -> 'a * 'b * 'c
val sub_arg : str * string * mode -> 'a * string * 'b -> arg -> arg
val subst_list :
  str * string * mode -> 'a * string * 'b -> arg list -> arg list
val subst :
  str * string * mode -> 'a * string * 'b -> 'c st_expr -> 'c st_expr
val subst_branches :
  str * string * mode -> 'a * string * 'b -> 'c branches -> 'c branches
val subst_aug :
  str * string * mode -> 'a * string * 'b -> 'c st_aug_expr -> 'c st_aug_expr
val subst_ctx :
  (str * string * mode) list ->
  ('a * string * 'b) list -> 'c st_expr -> 'c st_expr
val msubst : str * string * mode -> 'a * string * 'b -> msg -> msg
val fsubst :
  str * string * mode -> 'a * string * 'b -> 'c func_expr -> 'c func_expr
val fsubst_aug :
  str * string * mode ->
  'a * string * 'b -> 'c func_aug_expr -> 'c func_aug_expr
