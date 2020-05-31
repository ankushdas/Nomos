open Sexplib.Std
module R = Arith

type ext = Mark.ext option      (* optional extent (source region info) *)
[@@deriving sexp]

type potential =
  | Arith of R.arith            (* p,q, potential for work *)
  | Star                        (* potential to be inferred *)
[@@deriving sexp]

(* Functional Types *)

(* Session Types *)
type label = string [@@deriving sexp] (* l,k for internal and external choice *)
type tpname = string            (* v, for types defined with v = A *)
[@@deriving sexp]
type expname = string [@@deriving sexp] (* f, for processes defined with f = P *)


type func_tp =
  | Integer
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

type str =
    Hash
  | Dollar
[@@deriving sexp]

type chan = str * string * mode       (* channel name with modes *)
[@@deriving sexp]

type stype =
    Plus of choices                   (* +{l : Al} *)
  | With of choices                   (* &{l : Al} *)
  | PPlus of pchoices                 (* p+{l{p} : Al}*)
  | PWith of pchoices                 (* p&{l{p} : Al} *)
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
[@@deriving sexp]

and choices = (label * stype) list

and pchoices = (label * float * stype) list

type arglist =
  | Single of string * ext 
  | Curry of (string * ext) * arglist
[@@deriving sexp]

type arith_operator =
  | Add
  | Sub
  | Mult
  | Div
[@@deriving sexp]

type comp_operator =
  | Eq
  | Neq
  | Lt
  | Gt
  | Leq
  | Geq
[@@deriving sexp]

type rel_operator =
  | And
  | Or
[@@deriving sexp]

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
  | Str of string
  | Addr of string
  | Var of string
  | ListE of 'a func_aug_expr list
  | App of 'a func_aug_expr list
  | Cons of 'a func_aug_expr * 'a func_aug_expr
  | Match of 'a func_aug_expr * 'a func_aug_expr * string * string * 'a func_aug_expr
  | Lambda of arglist * 'a func_aug_expr
  | Op of 'a func_aug_expr * arith_operator * 'a func_aug_expr
  | CompOp of 'a func_aug_expr * comp_operator * 'a func_aug_expr
  | EqAddr of 'a func_aug_expr * 'a func_aug_expr
  | RelOp of 'a func_aug_expr * rel_operator * 'a func_aug_expr
  | Tick of potential * 'a func_aug_expr
  (* Nomos specific *)
  | GetTxnNum                                   (* Nomos.GetTxnNum(): get txn number *)
  | GetTxnSender                                (* Nomos.GetTxnSender(): get txn issuer *)
  | Command of 'a st_aug_expr
and 'a st_expr =
  (* judgmental constructs *)
  | Fwd of chan * chan                                      (* x <- y *)
  | Spawn of chan * expname *
    'a arg list * 'a st_aug_expr                            (* x <- f <- [y] ; Q *)
  | ExpName of chan * expname * 'a arg list                 (* x <- f <- [y] *)

  (* choice +{...} or &{...} *)
  | Lab of chan * label * 'a st_aug_expr                    (* x.k ; P *)
  | Case of chan * 'a branches                              (* case x (...) *)

  (* PNomos specific *)
  | PLab of chan * label * 'a st_aug_expr                       (* x..k  ; P *)
  | PCase of chan * 'a branches                                 (* pcase x (l => Ql) *)
  | Flip of float * 'a st_aug_expr * 'a st_aug_expr  (* flip {p} (H => Q | T => Q) *)

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

  (* Nomos specific, get channel from id *)
  | MakeChan of chan * stype * int * 'a st_aug_expr             (* #c : A <- Nomos.MakeChan(n) ; P *)
  | Abort                                                       (* abort *)
  | Print of printable list * 'a arg list * 'a st_aug_expr

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

and 'a branches = 'a branch list                          (* (l1 => P1 | ... | ln => Pn) *)

and 'a arg =
    STArg of chan
  | FArg of 'a func_expr
[@@deriving sexp]


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

type file = (string list * program) (* imports plus the actual program *)

type 'a value =
  | IntV of int
  | BoolV of bool
  | StrV of string
  | AddrV of string
  | ListV of 'a value list
  | LambdaV of arglist * 'a func_aug_expr
[@@deriving sexp]

type 'a msg =
    MLabI of chan * label * chan          (* c.k ; c <- c+ *)
  | MLabE of chan * label * chan          (* c.k ; c+ <- c *)
  | MSendT of chan * chan * chan          (* send c d ; c <- c+ *)
  | MSendL of chan * chan * chan          (* send c d ; c+ <- c *)
  | MClose of chan                        (* close c *)
  | MPayP of chan * potential * chan      (* pay c {p} ; c <- c+ *)
  | MPayG of chan * potential * chan      (* pay c {p} ; c+ <- c *)
  | MSendP of chan * 'a value * chan      (* send c M ; c <- c+ *)
  | MSendA of chan * 'a value * chan      (* send c M ; c+ <- c *)
[@@deriving sexp]

(* Environments *)

exception AstImpossible
exception UndeclaredTp;;

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
  | PPlus _ | PWith _
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

let sub_arg c' c a = match a with
    FArg s -> FArg s
  | STArg x -> STArg (sub c' c x);;

let eq_name (_s1,c1,_m1) (_s2,c2,_m2) = (c1 = c2);;

let subst_list c' c l = List.map (fun a -> sub_arg c' c a) l;; 

let rec subst c' c expr = match expr with
    Fwd(x,y) -> Fwd(sub c' c x, sub c' c y)
  | Spawn(x,f,xs,q) ->
      if eq_name c x
      then Spawn(x,f, subst_list c' c xs, q)
      else Spawn(x,f, subst_list c' c xs, subst_aug c' c q)
  | ExpName(x,f,xs) -> ExpName(x,f, subst_list c' c xs)
  | Lab(x,k,p) -> Lab(sub c' c x, k, subst_aug c' c p)
  | Case(x,branches) -> Case(sub c' c x, subst_branches c' c branches)
  | PLab(x,k,p) -> PLab(sub c' c x, k, subst_aug c' c p)
  | PCase(x,branches) -> PCase(x, subst_branches c' c branches)
  | Flip(pr,p1,p2) -> Flip(pr, subst_aug c' c p1, subst_aug c' c p2)
  | Send(x,w,p) -> Send(sub c' c x, sub c' c w, subst_aug c' c p)
  | Recv(x,y,p) ->
      if eq_name c y
      then Recv(sub c' c x, y, p)
      else Recv(sub c' c x, y, subst_aug c' c p)
  | Close(x) -> Close(sub c' c x)
  | Wait(x,q) -> Wait(sub c' c x, subst_aug c' c q)
  | Work(pot,p) -> Work(pot, subst_aug c' c p)
  | Pay(x,pot,p) -> Pay(sub c' c x, pot, subst_aug c' c p)
  | Get(x,pot,p) -> Get(sub c' c x, pot, subst_aug c' c p)
  | Acquire(x,y,p) ->
      if eq_name c y
      then Acquire(sub c' c x, y, p)
      else Acquire(sub c' c x, y, subst_aug c' c p)
  | Accept(x,y,p) ->
      if eq_name c y
      then Accept(sub c' c x, y, p)
      else Accept(sub c' c x, y, subst_aug c' c p)
  | Release(x,y,p) ->
      if eq_name c y
      then Release(sub c' c x, y, p)
      else Release(sub c' c x, y, subst_aug c' c p)
  | Detach(x,y,p) ->
      if eq_name c y
      then Detach(sub c' c x, y, p)
      else Detach(sub c' c x, y, subst_aug c' c p)
  | RecvF(x,y,p) -> RecvF(sub c' c x, y, subst_aug c' c p)
  | SendF(x,m,p) -> SendF(sub c' c x, m, subst_aug c' c p)
  | Let(x,e,p) -> Let(x, fsubst_aug c' c e, subst_aug c' c p)
  | IfS(e,p1,p2) -> IfS(fsubst_aug c' c e, subst_aug c' c p1, subst_aug c' c p2)
  | MakeChan(x,a,n,p) ->
      if eq_name c x
      then MakeChan(x, a, n, p)
      else MakeChan(x, a, n, subst_aug c' c p)
  | Abort -> Abort
  | Print(l,args,p) -> Print(l, subst_list c' c args, subst_aug c' c p)

and subst_branches c' c bs = match bs with
    [] -> []
  | (l,p)::bs' ->
      (l, subst_aug c' c p)::(subst_branches c' c bs')

and subst_aug c' c {st_structure = exp; st_data = d} =
  {st_structure = subst c' c exp; st_data = d}

and fsubst c' c fexp = match fexp with
    If(e1,e2,e3) -> If(fsubst_aug c' c e1, fsubst_aug c' c e2, fsubst_aug c' c e3)
  | LetIn(x,e1,e2) -> LetIn(x, fsubst_aug c' c e1, fsubst_aug c' c e2)
  | Bool _ | Int _ | Str _ | Addr _ | Var _ -> fexp
  | ListE(l) -> ListE(List.map (fsubst_aug c' c) l)
  | App(es) -> App(List.map (fsubst_aug c' c) es)
  | Cons(e1,e2) -> Cons(fsubst_aug c' c e1, fsubst_aug c' c e2)
  | Match(e1,e2,x,xs,e3) -> Match(fsubst_aug c' c e1, fsubst_aug c' c e2, x, xs, fsubst_aug c' c e3)
  | Lambda(xs,e) -> Lambda(xs, fsubst_aug c' c e)
  | Op(e1,op,e2) -> Op(fsubst_aug c' c e1, op, fsubst_aug c' c e2)
  | CompOp(e1,cop,e2) -> CompOp(fsubst_aug c' c e1, cop, fsubst_aug c' c e2)
  | EqAddr(e1,e2) -> EqAddr(fsubst_aug c' c e1, fsubst_aug c' c e2)
  | RelOp(e1,rop,e2) -> RelOp(fsubst_aug c' c e1, rop, fsubst_aug c' c e2)
  | Tick(pot,e) -> Tick(pot, fsubst_aug c' c e)
  | GetTxnNum -> GetTxnNum
  | GetTxnSender -> GetTxnSender
  | Command(p) -> Command(subst_aug c' c p)

and fsubst_aug c' c {func_structure = exp ; func_data = d} =
  {func_structure = fsubst c' c exp ; func_data = d};;

let rec toExpr d v = match v with
    IntV i -> Int i
  | BoolV b -> Bool b
  | StrV s -> Str s
  | AddrV a -> Addr a
  | ListV l -> ListE (List.map (fun x -> {func_structure = toExpr d x ; func_data = d}) l)
  | LambdaV(xs,e) -> Lambda (xs,e);;

let rec existsIn v xs = match xs with
    Single(x,_ext) -> v = x
  | Curry((x,_ext),xs') -> v = x || existsIn v xs';;

let rec substv v' v fexp = match fexp with
    If(e1,e2,e3) -> If(substv_aug v' v e1, substv_aug v' v e2, substv_aug v' v e3)
  | LetIn(x,e1,e2) ->
      if v = x
      then LetIn(x, substv_aug v' v e1, e2)
      else LetIn(x, substv_aug v' v e1, substv_aug v' v e2)
  | Bool _ | Str _ | Int _ | Addr _ -> fexp
  | Var x -> if x = v then v' else Var x
  | ListE(l) -> ListE(List.map (substv_aug v' v) l)
  | App(es) -> App(List.map (substv_aug v' v) es)
  | Cons(e1,e2) -> Cons(substv_aug v' v e1, substv_aug v' v e2)
  | Match(e1,e2,x,xs,e3) ->
      if v = x || v = xs
      then Match(substv_aug v' v e1, substv_aug v' v e2, x, xs, e3)
      else Match(substv_aug v' v e1, substv_aug v' v e2, x, xs, substv_aug v' v e3)
  | Lambda(xs,e) ->
      if existsIn v xs
      then Lambda(xs, e)
      else Lambda(xs, substv_aug v' v e)
  | Op(e1,op,e2) -> Op(substv_aug v' v e1, op, substv_aug v' v e2)
  | CompOp(e1,cop,e2) -> CompOp(substv_aug v' v e1, cop, substv_aug v' v e2)
  | EqAddr(e1,e2) -> EqAddr(substv_aug v' v e1, substv_aug v' v e2)
  | RelOp(e1,rop,e2) -> RelOp(substv_aug v' v e1, rop, substv_aug v' v e2)
  | Tick(pot,e) -> Tick(pot, substv_aug v' v e)
  | GetTxnNum -> GetTxnNum
  | GetTxnSender -> GetTxnSender
  | Command(p) -> Command(esubstv_aug v' v p)

and substv_aug v' v {func_structure = fexp ; func_data = d} =
  {func_structure = substv v' v fexp ; func_data = d}

and esubstv_aug v' v {st_structure = exp ; st_data = d} =
  {st_structure = esubstv v' v exp ; st_data = d}

and esubstv_arg v' v arg = match arg with
    FArg e -> FArg (substv v' v e)
  | STArg c -> STArg c

and esubstv v' v exp = match exp with
    Fwd(x,y) -> Fwd(x, y)
  | Spawn(x,f,xs,q) -> Spawn(x, f, List.map (fun arg -> esubstv_arg v' v arg) xs, esubstv_aug v' v q)
  | ExpName(x,f,xs) -> ExpName(x, f, List.map (fun arg -> esubstv_arg v' v arg) xs)
  | Lab(x,k,p) -> Lab(x, k, esubstv_aug v' v p)
  | Case(x,branches) -> Case(x, esubstv_branches v' v branches)
  | PLab(x,k,p) -> PLab(x, k, esubstv_aug v' v p)
  | PCase(x,branches) -> PCase(x, esubstv_branches v' v branches)
  | Flip(pr, p1, p2) -> Flip(pr, esubstv_aug v' v p1, esubstv_aug v' v p2)
  | Send(x,w,p) -> Send(x, w, esubstv_aug v' v p)
  | Recv(x,y,p) -> Recv(x, y, esubstv_aug v' v p)
  | Close(x) -> Close(x)
  | Wait(x,q) -> Wait(x, esubstv_aug v' v q)
  | Work(pot,p) -> Work(pot, esubstv_aug v' v p)
  | Pay(x,pot,p) -> Pay(x, pot, esubstv_aug v' v p)
  | Get(x,pot,p) -> Get(x, pot, esubstv_aug v' v p)
  | Acquire(x,y,p) -> Acquire(x, y, esubstv_aug v' v p)
  | Accept(x,y,p) -> Accept(x, y, esubstv_aug v' v p)
  | Release(x,y,p) -> Release(x, y, esubstv_aug v' v p)
  | Detach(x,y,p) -> Detach(x, y, esubstv_aug v' v p)
  | RecvF(x,y,p) ->
      if v = y
      then RecvF(x, y, p)
      else RecvF(x, y, esubstv_aug v' v p)
  | SendF(x,m,p) -> SendF(x, substv_aug v' v m, esubstv_aug v' v p)
  | Let(x,e,p) ->
      if v = x
      then Let(x, substv_aug v' v e, p)
      else Let(x, substv_aug v' v e, esubstv_aug v' v p)
  | IfS(e,p1,p2) -> IfS(substv_aug v' v e, esubstv_aug v' v p1, esubstv_aug v' v p2)
  | MakeChan(x,a,n,p) -> MakeChan(x, a, n, esubstv_aug v' v p)
  | Abort -> Abort
  | Print(l,args,p) -> Print(l, List.map (fun arg -> esubstv_arg v' v arg) args, esubstv_aug v' v p)

and esubstv_branches v' v bs = match bs with
    [] -> []
  | (l,p)::bs' ->
  (l, esubstv_aug v' v p)::(esubstv_branches v' v bs');;

let rec fsubst_ctx ctx' ctx expr = match ctx',ctx with
    (STArg c')::ctx', (STyped (c,_t))::ctx ->
      fsubst_aug c' c (fsubst_ctx ctx' ctx expr)
  | (FArg v')::ctx', (Functional (v,_t))::ctx ->
      substv_aug v' v (fsubst_ctx ctx' ctx expr)
  | [], [] -> expr
  | _c', _c -> raise AstImpossible;;

let msubst c' c m = match m with
    MLabI(x,k,y) -> MLabI(sub c' c x, k, sub c' c y)
  | MLabE(x,k,y) -> MLabE(sub c' c x, k, sub c' c y)
  | MSendT(x,w,y) -> MSendT(sub c' c x, w, sub c' c y)
  | MSendL(x,w,y) -> MSendL(sub c' c x, w, sub c' c y)
  | MClose(x) -> MClose(sub c' c x)
  | MPayP(x,pot,y) -> MPayP(sub c' c x, pot, sub c' c y)
  | MPayG(x,pot,y) -> MPayG(sub c' c x, pot, sub c' c y)
  | MSendP(x,v,y) -> MSendP(sub c' c x, v, sub c' c y)
  | MSendA(x,v,y) -> MSendA(sub c' c x, v, sub c' c y);;

exception SplitError

let split_last l =
  if List.length l <= 1
  then raise SplitError
  else
    let revl = List.rev l in
    match revl with
        [] -> raise AstImpossible
      | e::es -> (List.rev es, e);;
