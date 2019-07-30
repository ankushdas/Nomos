module R = Arith
type label = string
type tpname = string
type expname = string
type ext = Mark.ext option
type chan = string
type potential = R.arith
type stype =
    Plus of choices
  | With of choices
  | Tensor of stype * stype
  | Lolli of stype * stype
  | One
  | PayPot of potential * stype
  | GetPot of potential * stype
  | TpName of tpname
  | Up of stype
  | Down of stype
and choices = (label * stype) list
type chan_tp = chan * stype
type context = { shared : chan_tp list; linear : chan_tp list; }
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
val update_tp : 'a -> 'b -> ('a * 'b) list -> ('a * 'b) list
val lookup_expdec :
  decl_ext list -> expname -> (context * potential * chan_tp) option
val lookup_expdef : decl_ext list -> expname -> expression option
val lookup_choice : ('a * 'b) list -> 'a -> 'b option
val lookup_branch : branch list -> label -> expression option
val is_shared : stype -> bool
val strip_exts : expression -> expression
val strip_exts_branches : branches -> branches
type msg =
    MLab of chan * label * chan
  | MSend of chan * chan * chan
  | MClose of chan
  | MPay of chan * potential * chan
type sem =
    Proc of chan * int * (int * int) * expression
  | Msg of chan * int * (int * int) * msg
type config = Node of sem * config list | Leaf
val pp_pot : R.arith -> string
val pp_potpos : R.arith -> string
val pp_tp : stype -> tpname
val pp_choice : choices -> string
val pp_channames : string list -> string
val pp_chan : string * stype -> string
val pp_lsctx : (string * stype) list -> string
val pp_ctx : context -> string
val pp_exp : expression -> string
val pp_branches : branches -> string
exception AstUnsupported
val pp_decl : decl_ext -> string
val pp_msg : msg -> string
val pp_sem : sem -> string
val pp_config : config -> string
val pp_configs : config list -> string
