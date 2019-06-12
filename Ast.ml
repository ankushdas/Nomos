(* Abstract Syntax structure *)

module R = Arith

type label = string             (* l,k for internal and external choice *)
type tpname = string            (* v, for types defined with v = A *)
type expname = string           (* f, for processes defined with f = P *)
type ext = Mark.ext option      (* optional extent (source region info) *)
type chan = string              (* channel names *)

type potential = R.arith    (* p,q, potential for work *)

(* Types *)
type stype =
    Plus of choices                   (* +{...} *)
  | With of choices                   (* &{...} *)
  | Tensor of stype * stype           (* A x B *)
  | Lolli of stype * stype            (* A -o B *)
  | One                               (* 1 *)
  | PayPot of potential * stype       (* |> A  or  |{p}> A *)
  | GetPot of potential * stype       (* <| A  or  <{p}| A *)
  | TpName of tpname                  (* v *)

and choices = (label * stype) list

type chan_tp = chan * stype

type context = chan_tp list

(* Process Expressions *)
type expression =
  (* judgmental constructs *)
    Fwd of chan * chan                                                      (* x <- y *)
  | Spawn of chan * expname * chan list * expression                        (* x <- f <- [y] ; Q *)
  | ExpName of chan * expname * chan list                                   (* x <- f <- [y] *)

  (* choice +{...} or &{...} *)
  | Lab of chan * label * expression                    (* x.k ; P *)
  | Case of chan * branches                             (* case x (...) *)

  (* tensor or lolli *)
  | Send of chan * chan * expression                    (* send x w ; P *)
  | Recv of chan * chan * expression                    (* y <- recv x ; P *)

  (* termination 1 *)
  | Close of chan                                       (* close x *)
  | Wait of chan * expression                           (* wait x ; P *)

  (* work *)
  | Work of potential * expression                      (* work ; P or work{pot} ; P *)

  (* pay/get potential |{p}>A, |{p}>A *)
  | Pay of chan * potential * expression                (* pay x {pot} ; P *)
  | Get of chan * potential * expression                (* get x {pot} ; P *)

  (* mark with source region *)
  | Marked of expression Mark.marked

and branch =
  {
    lab_exp : label * expression;
    exp_extent : ext
  }

and branches = branch list          (* (l1 => P1 | ... | ln => Pn) *)

(* Declarations *)
type decl =
    Pragma of string * string                         (* #options, #test *)
  | TpDef of tpname * stype                           (* type a = A *)
  | TpEq of stype * stype                             (* eqtype a = b *)
  | ExpDecDef of expname *
    (context * potential * chan_tp) *                 (* proc f : Delta |{p}- c : C = expression *)
    expression
  | Exec of expname                                   (* exec f *)

type decl_ext =
  {
    declaration : decl;
    decl_extent : ext
  }

type environment = decl_ext list

(* Environments *)

exception AstImpossible

let rec lookup_tp decls v = match decls with
    {declaration = TpDef(v',a); decl_extent = _ext}::decls' ->
      if v = v' then Some a else lookup_tp decls' v
  | _decl::decls' -> lookup_tp decls' v
  | [] -> None;;

let expd_tp env v = match lookup_tp env v with
    Some a -> a
  | None -> raise AstImpossible;;

let rec update_tp x t ctx = match ctx with
    [] -> []
  | (y,a)::ctx' ->
    if x = y
    then (x,t)::ctx'
    else (y,a)::(update_tp x t ctx');;

let rec lookup_expdec decls f = match decls with
    {declaration = ExpDecDef(f',(ctx, pot, zc),_p); decl_extent = _ext}::decls' ->
      if f = f' then Some (ctx,pot,zc) else lookup_expdec decls' f
  | _decl::decls' -> lookup_expdec decls' f
  | [] -> None;;

let rec lookup_expdef decls f = match decls with
    {declaration = ExpDecDef(f',_dec,p); decl_extent = _ext}::decls' ->
      if f = f' then Some p else lookup_expdef decls' f
  | _decl::decls' -> lookup_expdef decls' f
  | [] -> None;;

let rec lookup_choice cs k = match cs with
    (l,a)::choices' ->
      if k = l then Some a
      else lookup_choice choices' k
  | [] -> None;;

let rec lookup_branch bs k = match bs with 
    {lab_exp = (l,p); _}::branches' ->
      if k = l then Some p
      else lookup_branch branches' k
  | [] -> None;;

(*************************)
(* Operational Semantics *)
(*************************)

(* strip_exts P = P' strips all source location information from P
 * This helps in writing the operational rules by pattern matching
 *)
let rec strip_exts expr = match expr with
    Fwd(x, y) -> Fwd(x, y)
  | Spawn(x,f,xs,q) -> Spawn(x,f,xs,strip_exts q)
  | ExpName(x,f,xs) -> ExpName(x,f,xs)
  | Lab(x,k,p) -> Lab(x, k, strip_exts p)
  | Case (x,branches) -> Case (x, strip_exts_branches branches)
  | Send(x,w,p) -> Send(x, w, strip_exts p)
  | Recv(x,y,p) -> Recv(x, y, strip_exts p)
  | Close(x) -> Close(x)
  | Wait(x,q) -> Wait(x, strip_exts q)
  | Work(pot,p) -> Work(pot,strip_exts p)
  | Pay(x,pot,p) -> Pay(x,pot,strip_exts p)
  | Get(x,pot,p) -> Get(x,pot,strip_exts p)
  | Marked(marked_p) -> strip_exts (Mark.data marked_p)

and strip_exts_branches bs = match bs with
    [] -> []
  | {lab_exp = (l,p); exp_extent = ext}::bs' ->
      {lab_exp = (l,strip_exts p); exp_extent = ext}::strip_exts_branches bs';;

type msg =
    MLab of chan * label * chan          (* c.k ; c <- c' *)
  | MSend of chan * chan * chan          (* send c d ; c <- c' *)
  | MClose of chan                       (* close c *)
  | MPay of chan * potential * chan      (* pay c {p} ; c <- c' *)
  
type sem = Proc of chan * int * (int * int) * expression   (* Proc(chan, time, (work, pot), P) *)
         | Msg of chan * int * (int * int) * msg           (* Msg(chan, time, (work, pot), M) *)

type config =
    Node of sem * (config list)
  | Leaf


(************)
(* Printing *)
(************)

let pp_pot e = match e with
    R.Int(0) -> ""
  | e -> "{" ^ R.pp_arith e ^ "}";;

let pp_potpos e = match e with
    R.Int(1) -> ""
  | e -> "{" ^ R.pp_arith e ^ "}";;

let rec pp_tp a = match a with
    (One) -> "1"
  | Plus(choice) -> "+{ " ^ pp_choice choice ^ " }"
  | With(choice) -> "&{ " ^ pp_choice choice ^ " }"
  | Tensor(a,b) -> pp_tp a ^ " * " ^ pp_tp b
  | Lolli(a,b) -> pp_tp a ^ " -o " ^ pp_tp b
  | GetPot(pot,a) -> "<" ^ pp_potpos pot ^ "| " ^ pp_tp a
  | PayPot(pot,a) -> "|" ^ pp_potpos pot ^ "> " ^ pp_tp a
  | TpName(a) -> a

and pp_choice cs = match cs with
    [] -> ""
  | [(l,a)] -> l ^ " : " ^ pp_tp a
  | (l,a)::cs' ->
      l ^ " : " ^ pp_tp a ^ ", " ^ pp_choice cs';;

let rec pp_channames chans = match chans with
  | [] -> ""
  | [c] -> c
  | c::chans' -> c ^ " " ^ pp_channames chans';;

let pp_chan (c,a) = "(" ^ c ^ " : " ^ pp_tp a ^ ")";;

let rec pp_ctx delta = match delta with
    [] -> ""
  | [(c,a)] -> pp_chan (c,a)
  | (c,a)::delta' -> pp_chan (c,a) ^ ", " ^ pp_ctx delta';;

let rec pp_exp p = match p with
    Fwd(x,y) -> x ^ " <- " ^ y
  | Spawn(x,f,xs,q) -> x ^ " <- " ^ f ^ " <- " ^ pp_channames xs ^ " ; " ^ pp_exp q
  | ExpName(x,f,xs) -> x ^ " <- " ^ f ^ " <- " ^ pp_channames xs
  | Lab(x,k,p) -> x ^ "." ^ k ^ " ; " ^ pp_exp p
  | Case (x,bs) -> "case " ^ x ^ " (" ^ pp_branches bs ^ ")"
  | Send(x,w,p) -> "send " ^ x ^ " " ^ w ^ " ; " ^ pp_exp p
  | Recv(x,y,p) -> y ^ " <- recv " ^ x ^ " ; " ^ pp_exp p
  | Close(x) -> "close " ^ x
  | Wait(x,p) -> "wait " ^ x ^ " ; " ^ pp_exp p
  | Work(pot,p) -> "work " ^ pp_potpos pot ^ " ; " ^ pp_exp p
  | Pay(x,pot,p) -> "pay " ^ x ^ " " ^ pp_potpos pot ^ " ; " ^ pp_exp p
  | Get(x,pot,p) -> "get " ^ x ^ " " ^ pp_potpos pot ^ " ; " ^ pp_exp p
  | Marked(marked_p) -> pp_exp (Mark.data marked_p)

and pp_branches bs = match bs with
    [] -> ""
  | [{lab_exp = (l,p); exp_extent = _ext}] -> l ^ " => " ^ pp_exp p
  | {lab_exp = (l,p); exp_extent = _ext}::bs' ->
      l ^ " => " ^ pp_exp p ^ " | " ^ pp_branches bs';;

exception AstUnsupported

let pp_decl dcl = match dcl.declaration with
    TpDef(v,a) -> "type " ^ v ^ " = " ^ pp_tp a
  | TpEq(TpName(a),TpName(a')) -> "eqtype " ^ a ^ " = " ^ a'
  | ExpDecDef(f,(delta,pot,(c,a)),p) -> "proc " ^ f ^ " : " ^
                                        pp_ctx delta ^ " |" ^ pp_pot pot ^ "- " ^ pp_chan (c,a) ^
                                        " = " ^ pp_exp p
  | Exec(f) -> "exec " ^ f
  | Pragma(p,line) -> p ^ line
  | TpEq(_a,_a') -> raise AstUnsupported;;

let pp_msg m = match m with
    MLab(c,k,c') -> c ^ "." ^ k ^ " ; " ^ pp_exp (Fwd(c,c'))
  | MSend(c,e,c') -> "send " ^ c ^ " " ^ e ^ " ; " ^ pp_exp (Fwd(c,c'))
  | MClose(c) -> "close " ^ c
  | MPay(c,pot,c') -> "pay " ^ c ^ " " ^ pp_potpos pot ^ " ; " ^ pp_exp (Fwd(c,c'));;

let pp_sem semobj = match semobj with
    Proc(c,t,(w,pot),p) ->
      "proc(" ^ c ^ ", " ^ string_of_int t ^ "(" ^ string_of_int w ^
      ", " ^ string_of_int pot ^ "), " ^ pp_exp p ^ ")"
  | Msg(c,t,(w,pot),m) ->
      "msg(" ^ c ^ ", " ^ string_of_int t ^ "(" ^ string_of_int w ^
      ", " ^ string_of_int pot ^ "), " ^ pp_msg m ^ ")";;

let rec pp_config conf = match conf with
    Leaf -> ""
  | Node(s,confs) -> pp_sem s ^ "[" ^ pp_configs confs ^ "]"

and pp_configs confs = match confs with
    [] -> ""
  | conf::confs' -> pp_config conf ^ " $ " ^ pp_configs confs';;