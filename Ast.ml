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
  | Up of stype                       (* /\ A *)
  | Down of stype                     (* \/ A *)

and choices = (label * stype) list

type chan_tp = chan * stype

type context =
  {
    shared: chan_tp list;
    linear: chan_tp list;
    ordered: chan_tp list
  }

(* Process Expressions *)
type expression =
  (* judgmental constructs *)
    Fwd of chan * chan                                  (* x <- y *)
  | Spawn of chan * expname *
    chan list * expression                              (* x <- f <- [y] ; Q *)
  | ExpName of chan * expname * chan list               (* x <- f <- [y] *)

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

  (* acquire/accept *)
  | Acquire of chan * chan * expression                 (* y <- acquire x *)
  | Accept of chan * chan * expression                  (* y <- accept x *)

  (* release/detach *)
  | Release of chan * chan * expression                 (* y <- release x *)
  | Detach of chan * chan * expression                  (* y <- detach x *)

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
    Pragma of string * string                 (* #options, #test *)
  | TpDef of tpname * stype                   (* type a = A *)
  | TpEq of stype * stype                     (* eqtype a = b *)
  | ExpDecDef of expname *
    (context * potential * chan_tp) *         (* proc f : Delta |{p}- c : C = expression *)
    expression
  | Exec of expname                           (* exec f *)

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

let rec updatetp x t ctx = match ctx with
    [] -> []
  | (y,a)::ctx' ->
    if x = y
    then (x,t)::ctx'
    else (y,a)::(updatetp x t ctx');;

let update_tp x t delta =
  let {shared = sdelta ; linear = ldelta ; ordered = odelta} = delta in
  {shared = updatetp x t sdelta ; linear = updatetp x t ldelta ; ordered = updatetp x t odelta};;

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

let rec is_shared env tp = match tp with
    Up _ -> true
  | TpName(v) -> is_shared env (expd_tp env v)
  | Plus _ | With _
  | Tensor _ | Lolli _
  | One
  | PayPot _ | GetPot _
  | Down _ -> false;;

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
  | Acquire(x,y,p) -> Acquire(x,y,strip_exts p)
  | Accept(x,y,p) -> Accept(x,y,strip_exts p)
  | Release(x,y,p) -> Release(x,y,strip_exts p)
  | Detach(x,y,p) -> Detach(x,y,strip_exts p)
  | Marked(marked_p) -> strip_exts (Mark.data marked_p)

and strip_exts_branches bs = match bs with
    [] -> []
  | {lab_exp = (l,p); exp_extent = ext}::bs' ->
      {lab_exp = (l,strip_exts p); exp_extent = ext}::strip_exts_branches bs';;

let sub c' c x =
  if x = c then c' else x;;

let rec subst_list c' c l = match l with
    [] -> []
  | x::xs ->
      if x = c
      then c'::(subst_list c' c xs)
      else x::(subst_list c' c xs);;

let rec subst c' c expr = match expr with
    Fwd(x,y) -> Fwd(sub c' c x, sub c' c y)
  | Spawn(x,f,xs,q) -> Spawn(x,f, subst_list c' c xs, subst c' c q)
  | ExpName(x,f,xs) -> ExpName(x,f, subst_list c' c xs)
  | Lab(x,k,p) -> Lab(sub c' c x, k, subst c' c p)
  | Case(x,branches) -> Case(sub c' c x, subst_branches c' c branches)
  | Send(x,w,p) -> Send(sub c' c x, sub c' c w, subst c' c p)
  | Recv(x,y,p) -> Recv(sub c' c x, y, subst c' c p)
  | Close(x) -> Close(sub c' c x)
  | Wait(x,q) -> Wait(sub c' c x, subst c' c q)
  | Work(pot,p) -> Work(pot, subst c' c p)
  | Pay(x,pot,p) -> Pay(sub c' c x, pot, subst c' c p)
  | Get(x,pot,p) -> Get(sub c' c x, pot, subst c' c p)
  | Acquire(x,y,p) -> Acquire(sub c' c x, y, subst c' c p)
  | Accept(x,y,p) -> Accept(sub c' c x, y, subst c' c p)
  | Release(x,y,p) -> Release(sub c' c x, y, subst c' c p)
  | Detach(x,y,p) -> Detach(sub c' c x, y, subst c' c p)
  | Marked(marked_p) -> Marked(subst c' c (Mark.data marked_p), Mark.ext marked_p)

and subst_branches c' c bs = match bs with
    [] -> []
  | {lab_exp = (l,p); exp_extent = ext}::bs' ->
      {lab_exp = (l, subst c' c p); exp_extent = ext}::
      (subst_branches c' c bs')

type msg =
    MLab of chan * label * chan          (* c.k ; c <- c' *)
  | MSend of chan * chan * chan          (* send c d ; c <- c' *)
  | MClose of chan                       (* close c *)
  | MPay of chan * potential * chan      (* pay c {p} ; c <- c' *)
  