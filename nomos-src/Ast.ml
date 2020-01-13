module R = Arith

type ext = Mark.ext option      (* optional extent (source region info) *)

type potential =
  | Arith of R.arith            (* p,q, potential for work *)
  | Star                        (* potential to be inferred *)

(* Functional Types *)

(* Session Types *)
type label = string             (* l,k for internal and external choice *)
type tpname = string            (* v, for types defined with v = A *)
type expname = string           (* f, for processes defined with f = P *)


type func_tp =
  | Integer
  | Boolean
  | ListTP of func_tp * potential
  | Arrow of func_tp * func_tp
  | VarT of string;;

type mode =
    Shared
  | Linear
  | Transaction
  | Pure
  | Unknown
  | MVar of string;;

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
  | Single of string * ext 
  | Curry of (string * ext) * arglist;;

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

type arg =
    STArg of chan
  | FArg of string;;

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
  | ListE of 'a func_aug_expr list
  | App of 'a func_aug_expr list
  | Cons of 'a func_aug_expr * 'a func_aug_expr
  | Match of 'a func_aug_expr * 'a func_aug_expr * string * string * 'a func_aug_expr
  | Lambda of arglist * 'a func_aug_expr
  | Op of 'a func_aug_expr * arith_operator * 'a func_aug_expr
  | CompOp of 'a func_aug_expr * comp_operator * 'a func_aug_expr
  | RelOp of 'a func_aug_expr * rel_operator * 'a func_aug_expr
  | Tick of potential * 'a func_aug_expr
  | Command of 'a st_aug_expr
and 'a st_expr =
  (* judgmental constructs *)
  | Fwd of chan * chan                                      (* x <- y *)
  | Spawn of chan * expname *
    arg list * 'a st_aug_expr                               (* x <- f <- [y] ; Q *)
  | ExpName of chan * expname * arg list                    (* x <- f <- [y] *)

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
  | RecvF of chan * string * 'a st_aug_expr                     (* y = recv x ; P *)
  | SendF of chan * 'a func_aug_expr * 'a st_aug_expr           (* send x (M) ; P *)
  | Let of string * 'a func_aug_expr * 'a st_aug_expr           (* let x = M ; P *)
  | IfS of 'a func_aug_expr * 'a st_aug_expr * 'a st_aug_expr   (* if e then P else Q *)
and 'a branch = label * 'a st_aug_expr

and 'a branches = 'a branch list;;                          (* (l1 => P1 | ... | ln => Pn) *)


type parsed_expr = ext func_aug_expr
type typed_expr = func_tp func_aug_expr

type argument =
  | Functional of string * func_tp
  | STyped of chan * stype

type chan_tp = chan * stype
type context =
  {
    shared: chan_tp list;
    linear: chan_tp list;
    ordered: argument list
  }

type decl =
  | TpDef of tpname * stype                   (* type a = A *)
  | ExpDecDef of expname * mode *
    (context * potential * chan_tp) *         (* proc 'mode' f : Delta |{p}- c : C = expression *)
    parsed_expr
  | Exec of expname                           (* exec f *)

type program = (decl * ext) list * ext

type value =
  | IntV of int
  | BoolV of bool
  | ListV of value list
  | LambdaV of value_context * arglist * valued_expr

and value_context = (string * value) list
and valued_expr = value func_aug_expr

type msg =
    MLabI of chan * label * chan          (* c.k ; c <- c+ *)
  | MLabE of chan * label * chan          (* c.k ; c+ <- c *)
  | MSendT of chan * chan * chan          (* send c d ; c <- c+ *)
  | MSendL of chan * chan * chan          (* send c d ; c+ <- c *)
  | MClose of chan                        (* close c *)
  | MPayP of chan * potential * chan      (* pay c {p} ; c <- c+ *)
  | MPayG of chan * potential * chan      (* pay c {p} ; c+ <- c *)
  | MSendP of chan * valued_expr * chan   (* send c M ; c <- c+ *)
  | MSendA of chan * valued_expr * chan   (* send c M ; c+ <- c *)

(* Environments *)

exception AstImpossible
exception UndeclaredTp

let rec lookup_tp decls v = match decls with
    (TpDef(v',a), _)::decls' ->
      if v = v' then Some a else lookup_tp decls' v
  | (_decl, _)::decls' -> lookup_tp decls' v
  | [] -> None;;

let expd_tp env v = match lookup_tp env v with
    Some a -> a
  | None -> raise UndeclaredTp;;

let rec lookup_expdec decls f = match decls with
    (ExpDecDef(f',m,(ctx, pot, zc),_p), _)::decls' ->
      if f = f' then Some (ctx,pot,zc,m) else lookup_expdec decls' f
  | _decl::decls' -> lookup_expdec decls' f
  | [] -> None;;

let rec lookup_expdef decls f = match decls with
    (ExpDecDef(f',_m,_dec,p), _)::decls' ->
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
  | TpName(v) ->
      begin
        match lookup_tp env v with
            None -> raise UndeclaredTp
          | Some a -> is_shared env a
      end
  | Plus _ | With _
  | Tensor _ | Lolli _
  | One
  | PayPot _ | GetPot _
  | FArrow _ | FProduct _
  | Down _ -> false;;

(*************************)
(* Operational Semantics *)
(*************************)

let sub (sc',c',mc') (_sc,c,_mc) (sx,x,mx) =
  if x = c then (sc',c',mc') else (sx,x,mx);;

let rec subst_list c' c l = match l with
   [] -> []
  | x::xs -> (sub c' c x)::(subst_list c' c xs);;

let rec subst c' c expr = match expr with
    Fwd(x,y) -> Fwd(sub c' c x, sub c' c y)
  | Spawn(x,f,xs,q) -> Spawn(x,f, subst_list c' c xs, subst_aug c' c q)
  | ExpName(x,f,xs) -> ExpName(x,f, subst_list c' c xs)
  | Lab(x,k,p) -> Lab(sub c' c x, k, subst_aug c' c p)
  | Case(x,branches) -> Case(sub c' c x, subst_branches c' c branches)
  | Send(x,w,p) -> Send(sub c' c x, sub c' c w, subst_aug c' c p)
  | Recv(x,y,p) -> Recv(sub c' c x, y, subst_aug c' c p)
  | Close(x) -> Close(sub c' c x)
  | Wait(x,q) -> Wait(sub c' c x, subst_aug c' c q)
  | Work(pot,p) -> Work(pot, subst_aug c' c p)
  | Pay(x,pot,p) -> Pay(sub c' c x, pot, subst_aug c' c p)
  | Get(x,pot,p) -> Get(sub c' c x, pot, subst_aug c' c p)
  | Acquire(x,y,p) -> Acquire(sub c' c x, y, subst_aug c' c p)
  | Accept(x,y,p) -> Accept(sub c' c x, y, subst_aug c' c p)
  | Release(x,y,p) -> Release(sub c' c x, y, subst_aug c' c p)
  | Detach(x,y,p) -> Detach(sub c' c x, y, subst_aug c' c p)

and subst_branches c' c bs = match bs with
    [] -> []
  | {lab_exp = (l,p); exp_extent = ext}::bs' ->
    {lab_exp = (l, subst c' c p); exp_extent = ext}::
    (subst_branches c' c bs');;

let rec subst_ctx ctx' ctx expr = match ctx',ctx with
    c'::ctx', c::ctx ->
      subst c' c (subst_ctx ctx' ctx expr)
  | [], [] -> expr
  | _c', _c -> raise AstImpossible;;

let msubst c' c m = match m with
    MLabI(x,k,y) -> MLabI(sub c' c x, k, sub c' c y)
  | MLabE(x,k,y) -> MLabE(sub c' c x, k, sub c' c y)
  | MSendT(x,w,y) -> MSendT(sub c' c x, w, sub c' c y)
  | MSendL(x,w,y) -> MSendL(sub c' c x, w, sub c' c y)
  | MClose(x) -> MClose(sub c' c x)
  | MPayP(x,pot,y) -> MPayP(sub c' c x, pot, sub c' c y)
  | MPayG(x,pot,y) -> MPayG(sub c' c x, pot, sub c' c y);;
