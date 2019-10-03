(* Interface for Ast *)

module R = Arith

type label = string
type tpname = string
type expname = string
type ext = Mark.ext option
type chan = string

type 'a polypot =
  | Arith of R.arith (* p,q, potential for work *)
  | Star of 'a (* potential to be inferred *)

type void = |        
type potential = void polypot
type potstar = unit polypot

type stype =
    Plus of choices
  | With of choices
  | Tensor of stype * stype
  | Lolli of stype * stype
  | One
  | PayPot of potstar * stype
  | GetPot of potstar * stype
  | TpName of tpname
  | Up of stype
  | Down of stype

and choices = (label * stype) list
type chan_tp = chan * stype
type context = {
  shared : chan_tp list;
  linear : chan_tp list;
  ordered : chan_tp list;
}

type expression =
    Fwd of chan * chan
  | Spawn of chan * expname * chan list * expression
  | ExpName of chan * expname * chan list
  | Lab of chan * label * expression
  | Case of chan * branches
  | Send of chan * chan * expression
  | Recv of chan * chan * expression
  | Close of chan
  | Wait of chan * expression
  | Work of potential * expression
  | Pay of chan * potential * expression
  | Get of chan * potential * expression
  | Acquire of chan * chan * expression
  | Accept of chan * chan * expression
  | Release of chan * chan * expression
  | Detach of chan * chan * expression
  | Marked of expression Mark.marked
and branch = { lab_exp : label * expression; exp_extent : ext; }
and branches = branch list

type decl =
    Pragma of string * string
  | TpDef of tpname * stype
  | TpEq of stype * stype
  | ExpDecDef of expname * (context * potential * chan_tp) * expression
  | Exec of expname
type decl_ext = { declaration : decl; decl_extent : ext; }
type environment = decl_ext list

exception AstImpossible

val lookup_tp : decl_ext list -> tpname -> stype option
val expd_tp : decl_ext list -> tpname -> stype
val update_tp : chan -> stype -> context -> context
val lookup_expdec :
  decl_ext list -> expname -> (context * potential * chan_tp) option
val lookup_expdef : decl_ext list -> expname -> expression option
val lookup_choice : ('a * 'b) list -> 'a -> 'b option
val is_shared : decl_ext list -> stype -> bool
val strip_exts : expression -> expression
val subst : chan -> chan -> expression -> expression
val subst_ctx : chan list -> chan list -> expression -> expression

type msg =
    MLabI of chan * label * chan
  | MLabE of chan * label * chan
  | MSendT of chan * chan * chan
  | MSendL of chan * chan * chan
  | MClose of chan
  | MPayP of chan * potential * chan
  | MPayG of chan * potential * chan

val msubst : chan -> chan -> msg -> msg
