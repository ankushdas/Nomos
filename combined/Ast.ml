module R = Arith

type potential =
  | Arith of R.arith            (* p,q, potential for work *)
  | Star                        (* potential to be inferred *)

(* Functional Types *)
type func_tp =
  | Integer
  | Boolean
  | ListTP of func_tp * potential
  | Arrow of func_tp * func_tp
  | VarT of string;;

(* Session Types *)
type label = string             (* l,k for internal and external choice *)
type tpname = string            (* v, for types defined with v = A *)
type expname = string           (* f, for processes defined with f = P *)

type mode =
    Shared
  | Linear
  | Transaction
  | Pure
  | Unknown
  | Var of string;;

type str =
    Hash
  | Dollar;;

type chan = str * string * mode       (* channel name with modes *)

type stype =
    Plus of choices                   (* +{...} *)
  | With of choices                   (* &{...} *)
  | Tensor of stype * stype * mode    (* A *[m] B *)
  | Lolli of stype * stype * mode     (* A -o[m] B *)
  | One                               (* 1 *)
  | PayPot of potential * stype       (* |> A  or  |{p}> A *)
  | GetPot of potential * stype       (* <| A  or  <{p}| A *)
  | TpName of tpname                  (* v *)
  | Up of stype                       (* /\ A *)
  | Down of stype                     (* \/ A *)
  | FArrow of func_tp * stype         (* t -> A *)
  | FProduct of func_tp * stype       (* t ^ A *)

and choices = (label * stype) list

type arglist =
  | Single of string
  | Curry of string * arglist;;

type arith_operator =
  | Add
  | Sub
  | Mult
  | Div;;

type comp_operator =
  | Eq
  | Neq
  | Lt
  | Gt
  | Leq
  | Geq;;

type rel_operator =
  | And
  | Or;;


type 'a func_aug_expr =
  {
    func_structure : 'a func_expr;
    func_data : 'a;
  }
and 'a st_aug_expr =
{
  st_structure : 'a st_expr;
  st_data : 'a;
}
and 'a func_expr =
  | If of 'a func_aug_expr * 'a func_aug_expr * 'a func_aug_expr
  | LetIn of string * 'a func_aug_expr * 'a func_aug_expr
  | Bool of bool
  | Int of int
  | Var of string
  | List of 'a func_aug_expr list
  | App of 'a func_aug_expr list
  | Cons of 'a func_aug_expr * 'a func_aug_expr
  | Match of 'a func_aug_expr * 'a func_aug_expr * string * string * 'a func_aug_expr
  | Lambda of arglist * 'a func_aug_expr
  | Op of 'a func_aug_expr * arith_operator * 'a func_aug_expr
  | CompOp of 'a func_aug_expr * comp_operator * 'a func_aug_expr
  | RelOp of 'a func_aug_expr * rel_operator * 'a func_aug_expr
  | Command of 'a st_aug_expr
and 'a st_expr =
  (* judgmental constructs *)
  | Fwd of chan * chan                                      (* x <- y *)
  | Spawn of chan * expname *
    chan list * 'a st_aug_expr                              (* x <- f <- [y] ; Q *)
  | ExpName of chan * expname * chan list                   (* x <- f <- [y] *)

  (* choice +{...} or &{...} *)
  | Lab of chan * label * 'a st_aug_expr                    (* x.k ; P *)
  | Case of chan * 'a branches                              (* case x (...) *)

  (* tensor or lolli *)
  | Send of chan * chan * 'a st_aug_expr                    (* send x w ; P *)
  | Recv of chan * chan * 'a st_aug_expr                    (* y <- recv x ; P *)

  (* termination 1 *)
  | Close of chan                                           (* close x *)
  | Wait of chan * 'a st_aug_expr                           (* wait x ; P *)

  (* work *)
  | Work of potential * 'a st_aug_expr                      (* work ; P or work{pot} ; P *)

  (* pay/get potential |{p}>A, |{p}>A *)
  | Pay of chan * potential * 'a st_aug_expr                (* pay x {pot} ; P *)
  | Get of chan * potential * 'a st_aug_expr                (* get x {pot} ; P *)

  (* acquire/accept *)
  | Acquire of chan * chan * 'a st_aug_expr                 (* y <- acquire x *)
  | Accept of chan * chan * 'a st_aug_expr                  (* y <- accept x *)

  (* release/detach *)
  | Release of chan * chan * 'a st_aug_expr                 (* y <- release x *)
  | Detach of chan * chan * 'a st_aug_expr                  (* y <- detach x *)

  (* arrow and product *)
  | RecvF of chan * string * 'a st_aug_expr                 (* y <- recv x ; P *)
  | SendF of chan * 'a func_aug_expr * 'a st_aug_expr           (* send x (M) ; P *)
  | Let of string * 'a func_aug_expr * 'a st_aug_expr           (* let x = M ; P *)
and 'a branch = label * 'a st_aug_expr

and 'a branches = 'a branch list;;                          (* (l1 => P1 | ... | ln => Pn) *)


type parsed_expr = unit func_aug_expr
type typed_expr = func_tp func_aug_expr

type argument =
  | Functional of string * func_tp
  | STyped of chan * stype

type context = argument list
type chan_tp = chan * stype

type decl =
  | TpDef of tpname * stype                   (* type a = A *)
  | ExpDecDef of expname * mode *
    (context * potential * chan_tp) *         (* proc 'mode' f : Delta |{p}- c : C = expression *)
    parsed_expr
  | Exec of expname                           (* exec f *)

type program = decl list

type value =
  | IntV of int
  | BoolV of bool
  | ListV of value list
  | LambdaV of value_context * arglist * valued_expr

and value_context = (string * value) list
and valued_expr = value func_aug_expr

