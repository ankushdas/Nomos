module R = Arith
type ext = Mark.ext option [@@deriving sexp]
type potential = Arith of R.arith | Star [@@deriving sexp]
type label = string
type tpname = string [@@deriving sexp]
type expname = string
type func_tp =
    Integer
  | Boolean
  | String
  | Address
  | ListTP of func_tp * potential
  | Arrow of func_tp * func_tp
  | VarT of string
[@@deriving sexp]
type mode =
    Shared
  | Linear
  | Transaction
  | Pure
  | Unknown
  | MVar of string
[@@deriving sexp]
type str = Hash | Dollar
type chan = str * string * mode [@@deriving sexp]
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
  | Coin
and choices = (label * stype) list
[@@deriving sexp]
type arglist = Single of string * ext | Curry of (string * ext) * arglist
[@@deriving sexp]
type arith_operator = Add | Sub | Mult | Div
type comp_operator = Eq | Neq | Lt | Gt | Leq | Geq
type rel_operator = And | Or
type 'a func_aug_expr = { func_structure : 'a func_expr; func_data : 'a; }
and 'a st_aug_expr = { st_structure : 'a st_expr; st_data : 'a; }
and 'a func_expr =
    If of 'a func_aug_expr * 'a func_aug_expr * 'a func_aug_expr
  | LetIn of string * 'a func_aug_expr * 'a func_aug_expr
  | Bool of bool
  | Int of int
  | Str of string
  | Addr of string
  | Var of string
  | ListE of 'a func_aug_expr list
  | App of 'a func_aug_expr list
  | Cons of 'a func_aug_expr * 'a func_aug_expr
  | Match of 'a func_aug_expr * 'a func_aug_expr * string * string *
      'a func_aug_expr
  | Lambda of arglist * 'a func_aug_expr
  | Op of 'a func_aug_expr * arith_operator * 'a func_aug_expr
  | CompOp of 'a func_aug_expr * comp_operator * 'a func_aug_expr
  | EqAddr of 'a func_aug_expr * 'a func_aug_expr
  | RelOp of 'a func_aug_expr * rel_operator * 'a func_aug_expr
  | Tick of potential * 'a func_aug_expr
  | GetTxnNum
  | GetTxnSender
  | Command of 'a st_aug_expr
and 'a st_expr =
    Fwd of chan * chan
  | Spawn of chan * expname * 'a arg list * 'a st_aug_expr
  | ExpName of chan * expname * 'a arg list
  | Lab of chan * label * 'a st_aug_expr
  | Case of chan * 'a branches
  | Send of chan * chan * 'a st_aug_expr
  | Recv of chan * chan * 'a st_aug_expr
  | Close of chan
  | Wait of chan * 'a st_aug_expr
  | Work of potential * 'a st_aug_expr
  | Deposit of potential * 'a st_aug_expr
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
  | MakeChan of chan * stype * int * 'a st_aug_expr 
  | Abort
  | Print of printable list * 'a arg list * 'a st_aug_expr
  [@@deriving sexp]
and printable = 
    Word of string
  | PInt 
  | PBool 
  | PStr 
  | PAddr 
  | PChan
  | PNewline
  [@@deriving sexp]

and 'a branch = label * 'a st_aug_expr
and 'a branches = 'a branch list
and 'a arg = STArg of chan | FArg of 'a func_expr
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
type program = (decl * ext) list
type file = string list * program
type 'a value =
    IntV of int
  | BoolV of bool
  | StrV of string
  | AddrV of string
  | ListV of 'a value list
  | LambdaV of arglist * 'a func_aug_expr
type 'a msg =
    MLabI of chan * label * chan
  | MLabE of chan * label * chan
  | MSendT of chan * chan * chan
  | MSendL of chan * chan * chan
  | MClose of chan
  | MPayP of chan * potential * chan
  | MPayG of chan * potential * chan
  | MSendP of chan * 'a value * chan
  | MSendA of chan * 'a value * chan
[@@deriving sexp]
exception UndeclaredTp
val lookup_tp : (decl * 'a) list -> tpname -> stype option
val expd_tp : (decl * 'a) list -> tpname -> stype
val lookup_expdec :
  (decl * 'a) list ->
  expname -> (context * potential * chan_tp * mode) option
val lookup_expdef : (decl * 'a) list -> expname -> parsed_expr option
val lookup_choice : ('a * 'b) list -> 'a -> 'b option
val is_shared : (decl * 'a) list -> stype -> bool
val subst :
  str * string * mode -> 'a * string * 'b -> 'c st_expr -> 'c st_expr
val subst_aug :
  str * string * mode -> 'a * string * 'b -> 'c st_aug_expr -> 'c st_aug_expr
val fsubst_aug :
  str * string * mode ->
  'a * string * 'b -> 'c func_aug_expr -> 'c func_aug_expr
val toExpr : 'a -> 'a value -> 'a func_expr
val substv_aug :
  'a func_expr -> string -> 'a func_aug_expr -> 'a func_aug_expr
val esubstv_aug : 'a func_expr -> string -> 'a st_aug_expr -> 'a st_aug_expr
val fsubst_ctx :
  'a arg list -> argument list -> 'a func_aug_expr -> 'a func_aug_expr
val msubst : str * string * mode -> 'a * string * 'b -> 'c msg -> 'c msg
val split_last : 'a list -> 'a list * 'a
