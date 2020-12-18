module C = Core
module R = Arith
module A = Ast
module PP = Pprint
module M = C.Map
module F = NomosFlags
module G = GasAcct
module EM = ErrorMsg
open Sexplib.Std

exception InsufficientPotential (* not enough potential to work or pay *)

exception UnconsumedPotential (* too much potential during termination *)

exception PotentialMismatch (* mismatch while spawning or exchanging *)

exception MissingBranch (* missing branch during internal/external choice *)

exception ChannelMismatch (* channel name mismatch in sem *)

exception UndefinedProcess (* spawning an undefined process *)

exception ExecImpossible (* should never happen at runtime *)

exception StarPotential (* star potential encountered at runtime *)

exception RuntimeError (* should never happen at runtime *)


type key =
    IntK of int
  | BoolK of bool
  | AddrK of string
  | StringK of string
[@@deriving sexp];;

module Key =
  struct
    module T =
      struct
        type t = key [@@deriving sexp]

        let compare x y =
          match x, y with
              IntK x, IntK y -> C.Int.compare x y
            | BoolK x, BoolK y -> C.Bool.compare x y
            | AddrK x, AddrK y -> C.String.compare x y
            | StringK x, StringK y -> C.String.compare x y
            | _, _ -> -1
      end
      include T
      include C.Comparable.Make(T)
  end

(* map from key to session-typed channel *)
type map_key_chan = A.chan Key.Map.t [@@deriving sexp]

(* map from  *)
type map_key_value = (A.ext A.value) Key.Map.t [@@deriving sexp]

type sem =
    (* Proc(procname, chan, in_use, time, (work, pot), P) *)
    Proc of string * A.chan * A.chan list * int * (int * int) * A.ext A.st_expr
    (* MapFProc(chan, in_use, time, (work, pot), map) *)
  | MapFProc of A.chan * A.chan list * int * (int * int) * map_key_value
    (* MapSTProc(chan, in_use, time, (work, pot), map) *)
  | MapSTProc of A.chan * A.chan list * int * (int * int) * map_key_chan
    (* Msg(chan, time, (work, pot), M) *)
  | Msg of A.chan * int * (int * int) * A.ext A.msg
  [@@deriving sexp];;

let pp_key k =
  match k with
      IntK i -> string_of_int i
    | BoolK b -> string_of_bool b
    | AddrK a -> a
    | StringK s -> s;;

let rec pp_kvlist d =
  match d with
      [] -> ""
    | [(k,v)] -> pp_key k ^ " -> " ^ PP.pp_val v 
    | (k,v)::d' -> pp_key k ^ " -> " ^ PP.pp_val v ^ " ; " ^ pp_kvlist d';;

let rec pp_kstlist d =
  match d with
      [] -> ""
    | [(k,v)] -> pp_key k ^ " -> " ^ PP.pp_chan v 
    | (k,v)::d' -> pp_key k ^ " -> " ^ PP.pp_chan v ^ " ; " ^ pp_kstlist d';;

let pp_fmap m =
  let d = M.to_alist m in
  "[" ^ pp_kvlist d ^ "]";;

let pp_stmap m =
  let d = M.to_alist m in
  "[" ^ pp_kstlist d ^ "]";;

let pp_sem sem = match sem with
    Proc(f,c,in_use,t,(w,pot),p) ->
      f ^ ": proc(" ^ PP.pp_chan c ^ ", in_use = [" ^ PP.pp_channames in_use ^
      "], t = " ^ string_of_int t ^ ", (w = " ^ string_of_int w ^
      ", pot = " ^ string_of_int pot ^ "), " ^ PP.pp_exp_prefix p ^ ")"
  | MapFProc(c,in_use,t,(w,pot),m) ->
      "fmap(" ^ PP.pp_chan c ^ ", in_use = [" ^ PP.pp_channames in_use ^
      "], t = " ^ string_of_int t ^ ", (w = " ^ string_of_int w ^
      ", pot = " ^ string_of_int pot ^ "), " ^ pp_fmap m ^ ")"
  | MapSTProc(c,in_use,t,(w,pot),m) ->
      "stmap(" ^ PP.pp_chan c ^ ", in_use = [" ^ PP.pp_channames in_use ^
      "], t = " ^ string_of_int t ^ ", (w = " ^ string_of_int w ^
      ", pot = " ^ string_of_int pot ^ "), " ^ pp_stmap m ^ ")"
  | Msg(c,t,(w,pot),m) ->
      "msg(" ^ PP.pp_chan c ^ ", t = " ^ string_of_int t ^ ", (w = " ^ string_of_int w ^
      ", pot = " ^ string_of_int pot ^ "), " ^ PP.pp_msg m ^ ")";;

let apply_op op n1 n2 =
  match op with
      A.Add -> n1 + n2
    | A.Sub -> n1 - n2
    | A.Mult -> n1 * n2
    | A.Div -> n1 / n2;;

let compare_op cop n1 n2 =
  match cop with
      A.Eq -> n1 = n2
    | A.Neq -> n1 <> n2
    | A.Lt -> n1 < n2
    | A.Gt -> n1 > n2
    | A.Leq -> n1 <= n2
    | A.Geq -> n1 >= n2;;

let relate_op rop b1 b2 =
  match rop with
      A.And -> b1 && b2
    | A.Or -> b1 || b2;;

let rec proj l = match l with
    [] -> ([], [])
  | (v,c)::l' ->
      let (vs, cs) = proj l' in
      (v::vs, c::cs);;

let txnNum = ref 0;;

let txnSender = ref "";;

let txnGas = ref 0;;

module Chan =
  struct
    module T =
      struct
        type t = A.chan [@@deriving sexp]

        let compare x y =
          let (_s1,c1,_m1) = x in
          let (_s2,c2,_m2) = y in
          C.String.compare c1 c2
      end
      include T
      include C.Comparable.Make(T)
  end

(* map from offered channel to semantic object *)
type map_chan_sem = sem Chan.Map.t [@@deriving sexp]

(* map from channel to its continuation *)
type map_chan_chan = A.chan Chan.Map.t [@@deriving sexp]

(* map from channel to its type *)
type map_chan_tp = A.stype Chan.Map.t [@@deriving sexp]

(* configuration type *)
(* map from offered channel to semantic object *)
(* map from channel to its continuation channel *)
(* map from linear channel to its shared counterpart *)
(* map from shared channels to their types *)
type configuration =
  { conf   : map_chan_sem;
    conts  : map_chan_chan;
    shared : map_chan_chan;
    types  : map_chan_tp;
    print_state : string
  } [@@deriving sexp]

type stepped_config =
    Changed of configuration
  | Unchanged of configuration
  | Aborted;;

let chan_num = ref 0;;

let get_str m = match m with
    A.Shared -> A.Hash
  | _ -> A.Dollar;;

let cfresh m =
  let n = !chan_num in
  let () = chan_num := n+1 in
  (get_str m, "ch" ^ (string_of_int n), m);;

let max(t,t') =
  if t > t' then t else t';;

let try_evaluate pot =
  match pot with
      A.Star -> raise StarPotential
    | A.Arith pot -> R.evaluate pot;;

let try_eq pot1 pot2 =
  match pot1, pot2 with
      A.Star, A.Star -> raise StarPotential
    | A.Star, A.Arith _ -> raise StarPotential
    | A.Arith _, A.Star -> raise StarPotential
    | A.Arith pot1, A.Arith pot2 -> (R.evaluate pot1) = (R.evaluate pot2);;

let rec find_branch l bs =
  match bs with
      (k,p)::bs' ->
        if k = l then p
        else find_branch l bs'
    | [] -> raise MissingBranch;;

let find_sem c config =
  match M.find config.conf c with
      None -> raise ExecImpossible
    | Some v -> v;;

type pol = Pos | Neg;;

let find_msg c { conf; conts; _ } dual =
  match dual with
      Neg ->
        begin
          match M.find conts c with
              None -> None
            | Some c' ->
                let sem_obj = M.find conf c' in
                match sem_obj with
                    None -> None
                  | Some (Proc _) -> None
                  | Some (MapFProc _) -> None
                  | Some (MapSTProc _) -> None
                  | Some (Msg _) -> sem_obj
        end
    | Pos ->
        let sem_obj = M.find conf c in
        match sem_obj with
            None -> None
          | Some (Proc _) -> None
          | Some (MapFProc _) -> None
          | Some (MapSTProc _) -> None
          | Some (Msg _) -> sem_obj;;

let find_fmap c { conf; _ } =
  let sem_obj = M.find conf c in
  match sem_obj with
      None -> None
    | Some (Proc _) -> None
    | Some (MapFProc _) -> sem_obj
    | Some (MapSTProc _) -> None
    | Some (Msg _) -> None;;

let find_stmap c { conf; _ } =
  let sem_obj = M.find conf c in
  match sem_obj with
      None -> None
    | Some (Proc _) -> None
    | Some (MapFProc _) -> None
    | Some (MapSTProc _) -> sem_obj
    | Some (Msg _) -> None;;

let find_map c { conf; _ } =
  let sem_obj = M.find conf c in
  match sem_obj with
      None -> None
    | Some (Proc _) -> None
    | Some (MapFProc _) -> sem_obj
    | Some (MapSTProc _) -> sem_obj
    | Some (Msg _) -> None;; 

let remove_sem c config =
  { config with conf = M.remove config.conf c };;

let index_chan sem = 
  match sem with
      Proc(_f,c,_in_use,_t,_wp,_p) -> c
    | MapFProc(c,_in_use,_t,_wp,_m) -> c
    | MapSTProc(c,_in_use,_t,_wp,_m) -> c
    | Msg(c,_t,_wp,_m) -> c;;

let add_sem sem config =
  let c = index_chan sem in
  { config with conf = M.add_exn config.conf ~key:c ~data:sem };;

let add_print s config =
  { config with print_state = config.print_state ^ s };;

let add_shared_map (c,c') config =
  { config with shared = M.add_exn config.shared ~key:c ~data:c' };;

let get_shared_chan c config =
  match M.find config.shared c with
      None -> raise ExecImpossible
    | Some c' -> c';;

let remove_shared_map c config =
  { config with shared = M.remove config.shared c };;

let add_cont (c,c') config =
  match M.find config.shared c with
      None -> { config with conts = M.add_exn config.conts ~key:c ~data:c' }
    | Some cs ->
        let config = remove_shared_map c config in
        let config = add_shared_map (c',cs) config in
        { config with conts = M.add_exn config.conts ~key:c ~data:c' };;

let remove_cont c config =
  { config with conts = M.remove config.conts c };;

let get_cont c config =
  match M.find config.conts c with
      None -> raise ExecImpossible
    | Some c' -> c';;

let add_chan_tp c t config =
  { config with types = M.add_exn config.types ~key:c ~data:t };;

let eq_name (_s1,c1,_m1) (_s2,c2,_m2) = c1 = c2;;

let uneq_name c1 c2 = not (eq_name c1 c2);;

let mode_of (_s,_c,m) = m;;

let mapsize config mp =
  let s = find_sem mp config in
  match s with
      MapFProc(_c,_in_use,_t,_wp,fmap) -> M.length fmap
    | MapSTProc(_c,_in_use,_t,_wp,stmap) -> M.length stmap
    | Proc _ | Msg _ -> raise RuntimeError;;

let rec eval config fexp = match fexp.A.func_structure with
    A.If(e1,e2,e3) ->
      begin
        let (v1, c1) = eval config e1 in
        match v1 with
            A.BoolV true ->
              let (v2, c2) = eval config e2 in
              (v2, R.plus c1 c2)
          | A.BoolV false ->
              let (v3, c3) = eval config e3 in
              (v3, R.plus c1 c3)
          | _ -> raise RuntimeError
      end
  | A.LetIn(x,e1,e2) ->
      begin
        let (v1, c1) = eval config e1 in
        let e2 = A.substv_aug (A.toExpr e1.func_data v1) x e2 in
        let (v2, c2) = eval config e2 in
        (v2, R.plus c1 c2)
      end
  | A.Bool b -> (A.BoolV b, R.Int 0)
  | A.Int i -> (A.IntV i, R.Int 0)
  | A.Str s -> (A.StrV s, R.Int 0)
  | A.Addr a -> (A.AddrV a, R.Int 0)
  | A.Var _ -> raise RuntimeError
  | A.ListE(l) ->
      begin
        let (vs, cs) = proj (List.map (eval config) l) in
        (A.ListV vs, List.fold_left (fun s x -> R.plus s x) (R.Int 0) cs)
      end
  | A.App(l) ->
      begin
        let (l', en) = A.split_last l in
        let (v', c') = eval config {A.func_structure = A.App(l') ; A.func_data = None} in
        let (vn, cn) = eval config en in
        match v' with
            A.LambdaV (xs, e) ->
              begin
                match xs with
                    A.Single (x,_ext) ->
                      let (v, c) = eval config (A.substv_aug (A.toExpr en.func_data vn) x e) in
                      (v, R.plus c (R.plus c' cn))
                  | A.Curry ((x,_ext),xs') -> (A.LambdaV(xs', A.substv_aug (A.toExpr en.func_data vn) x e), R.plus c' cn)
              end
          | _ -> raise RuntimeError
      end
  | A.Cons(e1,e2) ->
      begin
        let (v1, c1) = eval config e1 in
        let (v2, c2) = eval config e2 in
        match v2 with
            A.ListV l -> (A.ListV(v1::l), R.plus c1 c2)
          | _ -> raise RuntimeError
      end
  | A.Match(e1,e2,x,xs,e3) ->
      begin
        let (v1, c1) = eval config e1 in
        match v1 with
            A.ListV l ->
              begin
                match l with
                    [] ->
                      let (v2, c2) = eval config e2 in
                      (v2, R.plus c1 c2)
                  | v::vs ->
                      let e3 = A.substv_aug (A.toExpr None v) x e3 in
                      let e3 = A.substv_aug (A.toExpr None (A.ListV vs)) xs e3 in
                      let (v3, c3) = eval config e3 in
                      (v3, R.plus c1 c3)
              end
          | _ -> raise RuntimeError
      end
  | A.Lambda(xs,e) -> (A.LambdaV(xs,e), R.Int 0)
  | A.Op(e1,op,e2) ->
      begin
        let (v1, c1) = eval config e1 in
        let (v2, c2) = eval config e2 in
        match v1, v2 with
            A.IntV i1, A.IntV i2 -> (A.IntV (apply_op op i1 i2), R.plus c1 c2)
          | _, _ -> raise RuntimeError
      end
  | A.CompOp(e1,cop,e2) ->
      begin
        let (v1, c1) = eval config e1 in
        let (v2, c2) = eval config e2 in
        match v1, v2 with
            A.IntV i1, A.IntV i2 -> (A.BoolV (compare_op cop i1 i2), R.plus c1 c2)
          | _, _ -> raise RuntimeError
      end
  | A.EqAddr(e1,e2) ->
      begin
        let (v1, c1) = eval config e1 in
        let (v2, c2) = eval config e2 in
        match v1, v2 with
            A.AddrV a1, A.AddrV a2 -> (A.BoolV (a1 = a2), R.plus c1 c2)
          | _, _ -> raise RuntimeError
      end
  | A.RelOp(e1,rop,e2) ->
      begin
        let (v1, c1) = eval config e1 in
        let (v2, c2) = eval config e2 in
        match v1, v2 with
            A.BoolV b1, A.BoolV b2 -> (A.BoolV (relate_op rop b1 b2), R.plus c1 c2)
          | _, _ -> raise RuntimeError
      end
  | A.Tick(pot,e) ->
      begin
        let (v, c) = eval config e in
        match pot with
            A.Star -> raise RuntimeError
          | A.Arith p -> (v, R.plus c p)
      end
  | A.MapSize(mp) -> (A.IntV (mapsize config mp), R.Int 0)
  | A.GetTxnNum -> (A.IntV !txnNum, R.Int 0)
  | A.GetTxnSender -> (A.AddrV !txnSender, R.Int 0)
  | A.Command(_) -> raise RuntimeError;;


let fwd ch config =
  let s = find_sem ch config in
  match s with
      Proc(_f,c1,_in_use,t,(w,pot),A.Fwd(c2,d)) ->
        if uneq_name c1 c2
        then raise ChannelMismatch
        else
          begin
            (* try to apply fwd+ rule *)
            let msgp = find_msg d config Pos in
            match msgp with
                None ->
                  begin
                    (* try to apply fwd- rule *)
                    let msgn = find_msg c1 config Neg in
                    match msgn with
                      | Some(Msg(_d,t',(w',pot'),(A.MLabE _ as m)))
                      | Some(Msg(_d,t',(w',pot'),(A.MSendL _ as m)))
                      | Some(Msg(_d,t',(w',pot'),(A.MSendA _ as m)))
                      | Some(Msg(_d,t',(w',pot'),(A.MPayG _ as m))) ->
                          let config = remove_sem c1 config in
                          let e = get_cont c1 config in
                          let config = remove_sem e config in
                          let msg = Msg(e,max(t,t'),(w+w',pot+pot'),A.msubst d c1 m) in
                          let config = add_sem msg config in
                          let config = remove_cont c1 config in
                          let config = add_cont (d,e) config in
                          Changed config
                      | _s -> Unchanged config
                  end
              | Some(Msg(_d,t',(w',pot'),(A.MLabI _ as m)))
              | Some(Msg(_d,t',(w',pot'),(A.MSendT _ as m)))
              | Some(Msg(_d,t',(w',pot'),(A.MPayP _ as m)))
              | Some(Msg(_d,t',(w',pot'),(A.MSendP _ as m))) ->
                  let config = remove_sem c1 config in
                  let config = remove_sem d config in
                  let msg = Msg(c1,max(t,t'),(w+w',pot+pot'), A.msubst c1 d m) in
                  let config = add_sem msg config in
                  let d' = get_cont d config in
                  let config = remove_cont d config in
                  let config = add_cont (c1,d') config in
                  Changed config
              | Some(Msg(_d,t',(w',pot'),(A.MClose _ as m))) ->
                  let config = remove_sem c1 config in
                  let config = remove_sem d config in
                  let msg = Msg(c1,max(t,t'),(w+w',pot+pot'), A.msubst c1 d m) in
                  let config = add_sem msg config in
                  Changed config
              | _s -> Unchanged config
          end
    | _s -> raise ExecImpossible;;

let chan_mode env f =
  match A.lookup_expdec env f with
      None -> raise UndefinedProcess
    | Some(_ctx,_pot,_zc,m) -> m;;

let chan_tp env f =
  match A.lookup_expdec env f with
      None -> raise UndefinedProcess
    | Some(_ctx,_pot,zc,_m) -> let (_c, t) = zc in t;;

let add_chan ch in_use = ch::in_use;;

let replace_chan ch' ch l =
  if not (List.exists (eq_name ch) l)
  then raise ExecImpossible
  else List.map (fun other -> if eq_name ch other then ch' else other) l;;

let remove_chan ch l =
  match mode_of ch with
      A.Shared -> l
    | _m -> if not (List.exists (eq_name ch) l)
            then raise ExecImpossible
            else List.filter (uneq_name ch) l;;

let chans_diff chs1 chs2 =
  List.filter
    (fun x -> not (List.exists (eq_name x) chs2))
    chs1;;

let is_linear (_, _, m) =
  match m with
    A.Linear | A.Pure | A.Transaction -> true
  | A.Shared | A.Unknown | A.MVar _ -> false;;

let extract_chans l =
  List.filter_map
    (fun arg ->
       match arg with
         A.STArg c -> Some c
       | A.FArg _ -> None) l |>
  List.partition is_linear;;

let linear_chans = List.filter is_linear;;

let spawn env ch config =
  let s = find_sem ch config in
  let config = remove_sem ch config in
  match s with
      Proc(func,d,in_use,t,(w,pot),A.Spawn(x,f,xs,q)) ->
        let m = chan_mode env f in
        let c' = cfresh m in
        let pot' = try_evaluate (A.get_pot env f) in
        if pot < pot'
        then raise InsufficientPotential
        else
          let (linear_args, shared_args) = extract_chans xs in
          let f_in_use = linear_args @ shared_args in
          let func_in_use = add_chan c' (chans_diff in_use linear_args) in
          let proc1 = Proc(f,c',f_in_use,t+1,(0,pot'),A.ExpName(c',f,xs)) in
          let proc2 = Proc(func,d,func_in_use,t+1,(w,pot-pot'),A.subst c' x q.A.st_structure) in
          let config = add_sem proc1 config in
          let config = add_sem proc2 config in
          begin
            match m with
            | A.Shared -> Changed (add_chan_tp c' (chan_tp env f) config)
            | _ -> Changed config
          end
    | _s -> raise ExecImpossible;;

let get_stexp fexp = match fexp.A.func_structure with
    A.Command(exp) -> exp
  | _ -> raise ExecImpossible;;

let expd_def env x f xs =
  match A.lookup_expdef env f with
      None -> raise UndefinedProcess
    | Some(exp) ->
        match A.lookup_expdec env f with
            None -> raise ExecImpossible
          | Some (ctx,_pot,(z,_c),_m) ->
              let exp = A.fsubst_aug x z exp in
              let exp = A.fsubst_ctx xs ctx.ordered exp in
              let exp = get_stexp exp in
              exp;;

let expand env ch config =
  let s = find_sem ch config in
  let config = remove_sem ch config in
  match s with
      Proc(_func,c,in_use,t,(w,pot),A.ExpName(x,f,xs)) ->
        let p = expd_def env x f xs in
        let pot' = try_evaluate (A.get_pot env f) in
        if pot <> pot'
        then raise PotentialMismatch
        else
          let proc = Proc(f,c,in_use,t,(w,pot),A.subst c x p.A.st_structure) in
          let config = add_sem proc config in
          Changed config
    | _s -> raise ExecImpossible;;

let ichoice_S ch config =
  let s = find_sem ch config in
  let config = remove_sem ch config in
  match s with
      Proc(func,c1,in_use,t,wp,A.Lab(c2,l,p)) ->
        if uneq_name c1 c2
        then raise ChannelMismatch
        else
          let c' = cfresh (mode_of c1) in
          let msg = Msg(c1,t+1,(0,0),A.MLabI(c1,l,c')) in
          let proc = Proc(func,c',in_use,t+1,wp,A.subst c' c1 p.A.st_structure) in
          let config = add_sem msg config in
          let config = add_sem proc config in
          let config = add_cont (c1,c') config in
          Changed config
    | _s -> raise ExecImpossible;;

let ichoice_R ch config =
  let s = find_sem ch config in
  match s with
      Proc(func,d,in_use,t,(w,pot),A.Case(c,bs)) ->
        if not (uneq_name d c)
        then raise ChannelMismatch
        else
          let msg = find_msg c config Pos in
          begin
            match msg with
                Some(Msg(ceq, t', (w',pot'), A.MLabI(_ceq,l,c'))) ->
                  if uneq_name ceq c
                  then raise ChannelMismatch
                  else
                    let q = find_branch l bs in
                    let in_use' = replace_chan c' c in_use in
                    let proc = Proc(func,d,in_use',max(t,t')+1, (w+w',pot+pot'), A.subst c' c q.A.st_structure) in
                    let config = remove_sem ch config in
                    let config = remove_sem c config in
                    let config = add_sem proc config in
                    let config = remove_cont c config in
                    Changed config
              | _m -> Unchanged config
          end
    | _s -> raise ExecImpossible;;

let echoice_S ch config =
  let s = find_sem ch config in
  let config = remove_sem ch config in
  match s with
      Proc(func,d,in_use,t,wp,A.Lab(c,l,p)) ->
        if not (uneq_name d c)
        then raise ChannelMismatch
        else
          let c' = cfresh (mode_of c) in
          let msg = Msg(c',t+1,(0,0),A.MLabE(c,l,c')) in
          let in_use' = replace_chan c' c in_use in
          let proc = Proc(func,d,in_use',t+1,wp,A.subst c' c p.A.st_structure) in
          let config = add_sem msg config in
          let config = add_sem proc config in
          let config = add_cont (c,c') config in
          Changed config
    | _s -> raise ExecImpossible;;

let echoice_R ch config =
  let s = find_sem ch config in
  match s with
      Proc(func,c1,in_use,t,(w,pot),A.Case(c2,bs)) ->
        if uneq_name c1 c2
        then raise ChannelMismatch
        else
          let msg = find_msg c2 config Neg in
          begin
            match msg with
                Some(Msg(c2', t', (w',pot'), A.MLabE(c2eq,l,_c2'))) ->
                  if uneq_name c2eq c2
                  then raise ChannelMismatch
                  else
                    let q = find_branch l bs in
                    let proc = Proc(func,c2',in_use,max(t,t')+1, (w+w',pot+pot'), A.subst c2' c2 q.A.st_structure) in
                    let config = remove_sem ch config in
                    let config = remove_sem c2' config in
                    let config = add_sem proc config in
                    let config = remove_cont c2 config in
                    Changed config
              | _m -> Unchanged config
          end
    | _s -> raise ExecImpossible;;

let tensor_S ch config =
  let s = find_sem ch config in
  let config = remove_sem ch config in
  match s with
      Proc(func,c1,in_use,t,wp,A.Send(c2,e,p)) ->
        if uneq_name c1 c2
        then raise ChannelMismatch
        else
          let c' = cfresh (mode_of c1) in
          let msg = Msg(c1,t+1,(0,0),A.MSendT(c1,e,c')) in
          let in_use' = remove_chan e in_use in
          let proc = Proc(func,c',in_use',t+1,wp,A.subst c' c1 p.A.st_structure) in
          let config = add_sem msg config in
          let config = add_sem proc config in
          let config = add_cont (c1,c') config in
          Changed config
    | _s -> raise ExecImpossible;;

let tensor_R ch config =
  let s = find_sem ch config in
  match s with
      Proc(func,d,in_use,t,(w,pot),A.Recv(c,x,q)) ->
        if not (uneq_name d c)
        then raise ChannelMismatch
        else
          let msg = find_msg c config Pos in
          begin
            match msg with
                Some(Msg(ceq, t', (w',pot'), A.MSendT(_ceq,e,c'))) ->
                  if uneq_name ceq c
                  then raise ChannelMismatch
                  else
                    let q = A.subst_aug e x q in
                    let in_use' = add_chan e (replace_chan c' c in_use) in
                    let proc = Proc(func,d,in_use',max(t,t')+1, (w+w',pot+pot'), A.subst c' c q.A.st_structure) in
                    let config = remove_sem ch config in
                    let config = remove_sem c config in
                    let config = add_sem proc config in
                    let config = remove_cont c config in
                    Changed config
              | _m -> Unchanged config
          end
    | _s -> raise ExecImpossible;;
        
let lolli_S ch config =
  let s = find_sem ch config in
  let config = remove_sem ch config in
  match s with
      Proc(func,d,in_use,t,wp,A.Send(c,e,p)) ->
        if not (uneq_name d c)
        then raise ChannelMismatch
        else
          let c' = cfresh (mode_of c) in
          let msg = Msg(c',t+1,(0,0),A.MSendL(c,e,c')) in
          let in_use' = replace_chan c' c (remove_chan e in_use) in
          let proc = Proc(func,d,in_use',t+1,wp,A.subst c' c p.A.st_structure) in
          let config = add_sem msg config in
          let config = add_sem proc config in
          let config = add_cont (c,c') config in
          Changed config
    | _s -> raise ExecImpossible;;

let lolli_R ch config =
  let s = find_sem ch config in
  match s with
      Proc(func,c1,in_use,t,(w,pot),A.Recv(c2,x,q)) ->
        if uneq_name c1 c2
        then raise ChannelMismatch
        else
          let msg = find_msg c2 config Neg in
          begin
            match msg with
                Some(Msg(c2', t', (w',pot'), A.MSendL(c2eq,e,_c2'))) ->
                  if uneq_name c2eq c2
                  then raise ExecImpossible
                  else
                    let q = A.subst_aug e x q in
                    let in_use' = add_chan e in_use in
                    let proc = Proc(func,c2',in_use',max(t,t')+1, (w+w',pot+pot'), A.subst c2' c2 q.A.st_structure) in
                    let config = remove_sem ch config in
                    let config = remove_sem c2' config in
                    let config = add_sem proc config in
                    let config = remove_cont c2 config in
                    Changed config
              | _m -> Unchanged config
          end
    | _s -> raise ExecImpossible;;

let one_S ch config =
  let s = find_sem ch config in
  let config = remove_sem ch config in
  match s with
      Proc(_,c1,in_use,t,(w,pot),A.Close(c2)) ->
        if uneq_name c1 c2
        then raise ChannelMismatch
        else if pot > 0
        then raise UnconsumedPotential
        else if List.length (linear_chans in_use) > 0
        then raise ExecImpossible
        else
          let msg = Msg(c1,t+1,(w,pot),A.MClose(c1)) in
          let config = add_sem msg config in
          Changed config
    | _s -> raise ExecImpossible;;

let one_R ch config =
  let s = find_sem ch config in
  match s with
      Proc(func,d,in_use,t,(w,pot),A.Wait(c,q)) ->
        if not (uneq_name d c)
        then raise ChannelMismatch
        else
          let msg = find_msg c config Pos in
          begin
            match msg with
                Some(Msg(ceq, t', (w',pot'), A.MClose(_ceq))) ->
                  if uneq_name ceq c
                  then raise ChannelMismatch
                  else
                    let in_use' = remove_chan c in_use in
                    let proc = Proc(func,d,in_use',max(t,t')+1, (w+w',pot+pot'), q.A.st_structure) in
                    let config = remove_sem ch config in
                    let config = remove_sem c config in
                    let config = add_sem proc config in
                    Changed config
              | _m -> Unchanged config
          end
    | _s -> raise ExecImpossible;;

let work ch config =
  let s = find_sem ch config in
  let config = remove_sem ch config in
  match s with
      Proc(func,c,in_use,t,(w,pot),A.Work(k,p)) ->
        let k = try_evaluate k in
        if pot < k
        then raise InsufficientPotential
        else
          let proc = Proc(func,c,in_use,t+1,(w+k,pot-k),p.A.st_structure) in
          let config = add_sem proc config in
          Changed config
    | _s -> raise ExecImpossible;;

let deposit ch config =
  let s = find_sem ch config in
  let config = remove_sem ch config in
  match s with
      Proc(func,c,in_use,t,(w,pot),A.Deposit(k,p)) ->
        let k = try_evaluate k in
        if pot < k
        then raise InsufficientPotential
        else
          let proc = Proc(func,c,in_use,t+1,(w,pot-k),p.A.st_structure) in
          let () = txnGas := !txnGas + k in
          let config = add_sem proc config in
          Changed config
    | _s -> raise ExecImpossible;;

let paypot_S ch config =
  let s = find_sem ch config in
  let config = remove_sem ch config in
  match s with
      Proc(func,c1,in_use,t,(w,pot),A.Pay(c2,epot,p)) ->
        if uneq_name c1 c2
        then raise ChannelMismatch
        else if pot < try_evaluate epot
        then raise InsufficientPotential
        else
          let c' = cfresh (mode_of c1) in
          let vpot = try_evaluate epot in
          let msg = Msg(c1,t+1,(0,vpot),A.MPayP(c1,epot,c')) in
          let proc = Proc(func,c',in_use,t+1,(w,pot-vpot),A.subst c' c1 p.A.st_structure) in
          let config = add_sem msg config in
          let config = add_sem proc config in
          let config = add_cont (c1,c') config in
          Changed config
    | _s -> raise ExecImpossible;;

let paypot_R ch config =
  let s = find_sem ch config in
  match s with
      Proc(func,d,in_use,t,(w,pot),A.Get(c,epot,q)) ->
        if not (uneq_name d c)
        then raise ChannelMismatch
        else
          let msg = find_msg c config Pos in
          begin
            match msg with
                Some(Msg(ceq, t', (w',pot'), A.MPayP(_ceq,epot',c'))) ->
                  if uneq_name ceq c
                  then raise ChannelMismatch
                  else if not (try_eq epot epot')
                  then raise PotentialMismatch
                  else
                    let in_use' = replace_chan c' c in_use in
                    let proc = Proc(func,d,in_use',max(t,t')+1, (w+w',pot+pot'), A.subst c' c q.A.st_structure) in
                    let config = remove_sem ch config in
                    let config = remove_sem c config in
                    let config = add_sem proc config in
                    let config = remove_cont c config in
                    Changed config
              | _m -> Unchanged config
          end
    | _s -> raise ExecImpossible;;
        
let getpot_S ch config =
  let s = find_sem ch config in
  let config = remove_sem ch config in
  match s with
      Proc(func,d,in_use,t,(w,pot),A.Pay(c,epot,p)) ->
        if not (uneq_name d c)
        then raise ChannelMismatch
        else if pot < try_evaluate epot
        then raise InsufficientPotential
        else
          let c' = cfresh (mode_of c) in
          let vpot = try_evaluate epot in
          let msg = Msg(c',t+1,(0,vpot),A.MPayG(c,epot,c')) in
          let in_use' = replace_chan c' c in_use in
          let proc = Proc(func,d,in_use',t+1,(w,pot-vpot),A.subst c' c p.A.st_structure) in
          let config = add_sem msg config in
          let config = add_sem proc config in
          let config = add_cont (c,c') config in
          Changed config
    | _s -> raise ExecImpossible;;

let getpot_R ch config =
  let s = find_sem ch config in
  match s with
      Proc(func,c1,in_use,t,(w,pot),A.Get(c2,epot,q)) ->
        if uneq_name c1 c2
        then raise ChannelMismatch
        else
          let msg = find_msg c2 config Neg in
          begin
            match msg with
                Some(Msg(c2', t', (w',pot'), A.MPayG(c2eq,epot',_c2'))) ->
                  if uneq_name c2eq c2
                  then raise ChannelMismatch
                  else if not (try_eq epot epot')
                  then raise PotentialMismatch
                  else
                    let proc = Proc(func,c2',in_use,max(t,t')+1, (w+w',pot+pot'), A.subst c2' c2 q.A.st_structure) in
                    let config = remove_sem ch config in
                    let config = remove_sem c2' config in
                    let config = add_sem proc config in
                    let config = remove_cont c2 config in
                    Changed config
              | _m -> Unchanged config
          end
    | _s -> raise ExecImpossible;;

let get_sems config = M.data config.conf

let rec find_procs ch sems =
  match sems with
      [] -> []
    | (Proc(_,_c,_in_use,_t,_wp,A.Acquire(a,_x,_p)) as proc)::sems' ->
        if a = ch
        then proc::(find_procs ch sems')
        else find_procs ch sems'
    | _sem::sems' -> find_procs ch sems';;

let find_acquiring_procs ch config =
  let sems = get_sems config in
  find_procs ch sems;;

let pick_random l =
  match l with
      [] -> None
    | l ->
        let x = if !F.random then (Random.int (List.length l)) else 0 in
        Some(List.nth l x);;

let up ch config =
  let s = find_sem ch config in
  match s with
      Proc(func_acc,as1,in_use,t,wp,A.Accept(as2,x,p)) ->
        begin
          if uneq_name as1 as2
          then raise ChannelMismatch
          else
            let procs = find_acquiring_procs as1 config in
            (*
            let () = print_string ("picking acquiring proc for " ^ PP.pp_chan as1 ^ "\n") in
            let () = print_string (List.fold_left (fun s x -> s ^ "\n" ^ pp_sem x) "" procs) in
            *)
            let proc_opt = pick_random procs in
            match proc_opt with
                None -> Unchanged config
              | Some proc ->
                  (*
                  let () = print_string ("\npicked acquiring proc " ^ pp_sem proc ^ "\n") in
                  *)
                  match proc with
                      Proc(func_acq,c,in_use_acq,t',wp',A.Acquire(aseq,x',q)) ->
                        if uneq_name aseq as1
                        then raise ChannelMismatch
                        else
                          let al = cfresh A.Linear in
                          let proc1 = Proc(func_acc,al,in_use,max(t,t')+1,wp,A.subst al x p.A.st_structure) in
                          let in_use_acq' = add_chan al in_use_acq in
                          let proc2 = Proc(func_acq,c,in_use_acq',max(t,t')+1,wp',A.subst al x' q.A.st_structure) in
                          let config = remove_sem as1 config in
                          let config = remove_sem c config in
                          let config = add_sem proc1 config in
                          let config = add_sem proc2 config in
                          let config = add_shared_map (al,as1) config in
                          Changed config
                    | _s -> raise ExecImpossible
        end
    | _s -> raise ExecImpossible;;

let rec find_proc ch sems =
  match sems with
      [] -> None
    | (Proc(_,_c,_in_use,_t,_wp,A.Release(a,_x,_p)) as proc)::sems' ->
        if a = ch
        then Some proc
        else find_proc ch sems'
    | _sem::sems' -> find_proc ch sems';;

let find_releasing_proc ch config =
  let sems = get_sems config in
  find_proc ch sems;;

let down ch config =
  let s = find_sem ch config in
  match s with
      Proc(func_det,al1,in_use,t,wp,A.Detach(al2,x,p)) ->
        begin
          if uneq_name al1 al2
          then raise ChannelMismatch
          else
            let proc = find_releasing_proc al1 config in
            match proc with
                None -> Unchanged config
              | Some proc ->
                  match proc with
                      Proc(func_rel,c,in_use_rel,t',wp',A.Release(aleq,x',q)) ->
                        if uneq_name aleq al1
                        then raise ChannelMismatch
                        else
                          let ash = get_shared_chan al1 config in
                          let proc1 = Proc(func_det,ash,in_use,max(t,t')+1,wp,A.subst ash x p.A.st_structure) in
                          let in_use_rel' = remove_chan al1 in_use_rel in
                          let proc2 = Proc(func_rel,c,in_use_rel',max(t,t')+1,wp',A.subst ash x' q.A.st_structure) in
                          let config = remove_sem al1 config in
                          let config = remove_sem c config in
                          let config = add_sem proc1 config in
                          let config = add_sem proc2 config in
                          let config = remove_shared_map al1 config in
                          Changed config
                    | _s -> raise ExecImpossible
        end
    | _s -> raise ExecImpossible;;
    
let product_S ch config =
  let s = find_sem ch config in
  let config = remove_sem ch config in
  match s with
      Proc(func,c1,in_use,t,(w,pot),A.SendF(c2,e,p)) ->
        if uneq_name c1 c2
        then raise ChannelMismatch
        else
          let c' = cfresh (mode_of c1) in
          let (v, acost) = eval config e in
          let vcost = R.evaluate acost in
          let msg = Msg(c1,t+1,(0,0),A.MSendP(c1,v,c')) in
          let proc = Proc(func,c',in_use,t+1,(w+vcost,pot-vcost),A.subst c' c1 p.A.st_structure) in
          let config = add_sem msg config in
          let config = add_sem proc config in
          let config = add_cont (c1,c') config in
          Changed config
    | _s -> raise ExecImpossible;;
    
let product_R ch config =
  let s = find_sem ch config in
  match s with
      Proc(func,d,in_use,t,(w,pot),A.RecvF(c,x,q)) ->
        if not (uneq_name d c)
        then raise ChannelMismatch
        else
          let msg = find_msg c config Pos in
          begin
            match msg with
                Some(Msg(ceq, t', (w',pot'), A.MSendP(_ceq,v,c'))) ->
                  if uneq_name ceq c
                  then raise ChannelMismatch
                  else
                    let q = A.esubstv_aug (A.toExpr None v) x q in
                    let in_use' = replace_chan c' c in_use in
                    let proc = Proc(func,d,in_use',max(t,t')+1, (w+w',pot+pot'), A.subst c' c q.A.st_structure) in
                    let config = remove_sem ch config in
                    let config = remove_sem c config in
                    let config = add_sem proc config in
                    let config = remove_cont c config in
                    Changed config
              | _m -> Unchanged config
          end
    | _s -> raise ExecImpossible;;

let arrow_S ch config =
  let s = find_sem ch config in
  let config = remove_sem ch config in
  match s with
      Proc(func,d,in_use,t,(w,pot),A.SendF(c,e,p)) ->
        if not (uneq_name d c)
        then raise ChannelMismatch
        else
          let c' = cfresh (mode_of c) in
          (* let () = print_string ("trying to evaluate " ^ PP.pp_fexp () 0 e.A.func_structure ^ "\n") in *)
          let (v, acost) = eval config e in
          let vcost = R.evaluate acost in
          (* let () = print_string ("evaluated to: " ^ PP.pp_val v ^ "\n") in *)
          let msg = Msg(c',t+1,(0,0),A.MSendA(c,v,c')) in
          let in_use' = replace_chan c' c in_use in
          let proc = Proc(func,d,in_use',t+1,(w+vcost,pot-vcost),A.subst c' c p.A.st_structure) in
          let config = add_sem msg config in
          let config = add_sem proc config in
          let config = add_cont (c,c') config in
          Changed config
    | _s -> raise ExecImpossible;;

let arrow_R ch config =
  let s = find_sem ch config in
  match s with
      Proc(func,c1,in_use,t,(w,pot),A.RecvF(c2,x,q)) ->
        if uneq_name c1 c2
        then raise ChannelMismatch
        else
          let msg = find_msg c2 config Neg in
          begin
            match msg with
                Some(Msg(c2', t', (w',pot'), A.MSendA(c2eq,v,_c2'))) ->
                  if uneq_name c2eq c2
                  then raise ExecImpossible
                  else
                    let q = A.esubstv_aug (A.toExpr None v) x q in
                    let proc = Proc(func,c2',in_use,max(t,t')+1, (w+w',pot+pot'), A.subst c2' c2 q.A.st_structure) in
                    let config = remove_sem ch config in
                    let config = remove_sem c2' config in
                    let config = add_sem proc config in
                    let config = remove_cont c2 config in
                    Changed config
              | _m -> Unchanged config
          end
    | _s -> raise ExecImpossible;;
    
let letS ch config =
  let s = find_sem ch config in
  let config = remove_sem ch config in
  match s with
      Proc(func,c,in_use,t,(w,pot),A.Let(x,e,p)) ->
        let (v, acost) = eval config e in
        let vcost = R.evaluate acost in
        let p = A.esubstv_aug (A.toExpr None v) x p in
        let proc = Proc(func,c,in_use,t+1,(w+vcost,pot-vcost),p.A.st_structure) in
        let config = add_sem proc config in
        Changed config
    | _s -> raise ExecImpossible;;

let ifS ch config =
  let s = find_sem ch config in
  let config = remove_sem ch config in
  match s with
      Proc(func,c,in_use,t,(w,pot),A.IfS(e,p1,p2)) ->
        let (v, acost) = eval config e in
        let vcost = R.evaluate acost in
        begin
          match v with
              A.BoolV true ->
                let proc = Proc(func,c,in_use,t+1,(w+vcost,pot-vcost),p1.A.st_structure) in
                let config = add_sem proc config in
                Changed config
            | A.BoolV false ->
                let proc = Proc(func,c,in_use,t+1,(w+vcost,pot-vcost),p2.A.st_structure) in
                let config = add_sem proc config in
                Changed config
            | _ -> raise RuntimeError
        end
    | _s -> raise ExecImpossible;;

let fmapcreate ch config =
  let s = find_sem ch config in
  let config = remove_sem ch config in
  match s with
      Proc(func,c,in_use,t,(w,pot),A.FMapCreate(mp,kt,_vt,p)) ->
        let newch = cfresh (mode_of mp) in
        let in_use' = add_chan newch in_use in
        let p = A.subst_aug newch mp p in
        let map_proc = MapFProc(newch, [], 0, (0, 0), M.empty (module Key)) in
        let newproc = Proc(func, c, in_use', t+1, (w,pot),p.A.st_structure) in
        let config = add_sem map_proc config in
        let config = add_sem newproc config in
        Changed config
    | _s -> raise ExecImpossible;;

let stmapcreate ch config =
  let s = find_sem ch config in
  let config = remove_sem ch config in
  match s with
      Proc(func,c,in_use,t,(w,pot),A.STMapCreate(mp,_kt,_vt,p)) ->
        let newch = cfresh (mode_of mp) in
        let in_use' = add_chan newch in_use in
        let p = A.subst_aug newch mp p in
        let map_proc = MapSTProc(newch, [], 0, (0, 0), M.empty (module Key)) in
        let newproc = Proc(func, c, in_use', t+1, (w,pot),p.A.st_structure) in
        let config = add_sem map_proc config in
        let config = add_sem newproc config in
        Changed config
    | _s -> raise ExecImpossible;;

let make_key k = match k with
    A.IntV i -> IntK i
  | A.BoolV b -> BoolK b
  | A.StrV s -> StringK s
  | A.AddrV a -> AddrK a
  | A.ListV _ | A.LambdaV _ -> raise RuntimeError;;

let fmapinsert ch config =
  let s = find_sem ch config in
  let config = remove_sem ch config in
  match s with
      Proc(func, c, in_use, t, (w,pot), A.FMapInsert(mp,k,v,p)) ->
        let map_proc = find_fmap mp config in
        begin
          match map_proc with
              Some(MapFProc(mp', in_use', t', wp', fmap)) ->
                if uneq_name mp mp'
                then raise ExecImpossible
                else
                  let (kval, acostk) = eval config k in
                  let (vval, acostv) = eval config v in
                  let config = remove_sem mp config in
                  let vcostk = R.evaluate acostk in
                  let vcostv = R.evaluate acostv in
                  let vcost = vcostk + vcostv in
                  let newproc = Proc(func, c, in_use, max(t,t')+1, (w + vcost, pot - vcost), p.A.st_structure) in
                  let newmap = MapFProc(mp', in_use', max(t,t')+1, wp', M.add_exn fmap ~key:(make_key kval) ~data:vval) in
                  let config = add_sem newmap config in
                  let config = add_sem newproc config in
                  Changed config
            | _m -> raise ExecImpossible
        end
    | _s -> raise ExecImpossible;;

let stmapinsert ch config =
  let s = find_sem ch config in
  let config = remove_sem ch config in
  match s with
      Proc(func, c, in_use, t, (w,pot), A.STMapInsert(mp,k,v,p)) ->
        let map_proc = find_stmap mp config in
        begin
          match map_proc with
              Some(MapSTProc(mp', in_use', t', wp', stmap)) ->
                if uneq_name mp mp'
                then raise ExecImpossible
                else
                  let new_in_use = remove_chan v in_use in
                  let new_in_use' = add_chan v in_use' in
                  let (kval, acost) = eval config k in
                  let config = remove_sem mp config in
                  let vcost = R.evaluate acost in
                  let newproc = Proc(func, c, new_in_use, max(t,t')+1, (w + vcost, pot - vcost), p.A.st_structure) in
                  let newmap = MapSTProc(mp', new_in_use', max(t,t')+1, wp', M.add_exn stmap ~key:(make_key kval) ~data:v) in
                  let config = add_sem newmap config in
                  let config = add_sem newproc config in
                  Changed config
            | _m -> raise ExecImpossible
        end
    | _s -> raise ExecImpossible;;
        
exception KeyNotFound of string * string;;

let fmapdelete ch config =
  let s = find_sem ch config in
  let config = remove_sem ch config in
  match s with
      Proc(func, c, in_use, t, (w,pot), A.FMapDelete(y,mp,k,p)) ->
        let map_proc = find_fmap mp config in
        begin
          match map_proc with
              Some(MapFProc(mp', in_use', t', wp', fmap)) ->
                if uneq_name mp mp'
                then raise ExecImpossible
                else
                  let (kval, acost) = eval config k in
                  let key = make_key kval in
                  let vopt = M.find fmap key in
                  begin
                  match vopt with
                      None -> raise (KeyNotFound(PP.pp_val kval, PP.pp_chan mp))
                    | Some v ->
                        let config = remove_sem mp config in
                        let vcost = R.evaluate acost in
                        let p' = A.esubstv_aug (A.toExpr None v) y p in
                        let newproc = Proc(func, c, in_use, max(t,t')+1, (w + vcost, pot - vcost), p'.A.st_structure) in
                        let newmap = MapFProc(mp', in_use', max(t,t')+1, wp', M.remove fmap key) in
                        let config = add_sem newmap config in
                        let config = add_sem newproc config in
                        Changed config
                  end
            | _m -> raise ExecImpossible
        end
    | _s -> raise ExecImpossible;;

let stmapdelete ch config =
  let s = find_sem ch config in
  let config = remove_sem ch config in
  match s with
      Proc(func, c, in_use, t, (w,pot), A.STMapDelete(y,mp,k,p)) ->
        let map_proc = find_stmap mp config in
        begin
          match map_proc with
              Some(MapSTProc(mp', in_use', t', wp', stmap)) ->
                if uneq_name mp mp'
                then raise ExecImpossible
                else
                  let (kval, acost) = eval config k in
                  let key = make_key kval in
                  let vopt = M.find stmap key in
                  begin
                  match vopt with
                      None -> raise (KeyNotFound(PP.pp_val kval, PP.pp_chan mp))
                    | Some v ->
                        let new_in_use = add_chan v in_use in
                        let new_in_use' = remove_chan v in_use' in
                        let config = remove_sem mp config in
                        let vcost = R.evaluate acost in
                        let p' = A.subst_aug v y p in
                        let newproc = Proc(func, c, new_in_use, max(t,t')+1, (w + vcost, pot - vcost), p'.A.st_structure) in
                        let newmap = MapSTProc(mp', new_in_use', max(t,t')+1, wp', M.remove stmap key) in
                        let config = add_sem newmap config in
                        let config = add_sem newproc config in
                        Changed config
                  end
            | _m -> raise ExecImpossible
        end
    | _s -> raise ExecImpossible;;

let deletable mp =
  match (mode_of mp) with
      A.Shared -> true
    | A.Pure -> false
    | A.Transaction | A.Linear
    | A.Unknown | A.MVar _ -> raise ExecImpossible;;

let emp_status stmap =
  if M.length stmap = 0
  then "empty"
  else "nonempty";;

let mapclose ch config =
  let s = find_sem ch config in
  let config = remove_sem ch config in
  match s with
      Proc(func, c, in_use, t, (w,pot), A.MapClose(mp,p)) ->
        let map_proc = find_map mp config in
        begin
          match map_proc with
              Some(MapSTProc(mp', in_use', t', (w',pot'), stmap)) ->
                if uneq_name mp mp'
                then raise ExecImpossible
                else if (deletable mp)
                then
                  let config = remove_sem mp config in
                  let new_in_use = remove_chan mp in_use in
                  let newproc = Proc(func, c, new_in_use, max(t,t')+1, (w+w',pot+pot'), p.A.st_structure) in
                  let config = add_sem newproc config in
                  Changed config
                else if M.length stmap > 0
                then
                  let config = remove_sem mp config in
                  let mpn = cfresh (mode_of mp) in
                  let newmap = MapSTProc(mpn, in_use', max(t,t')+1, (w', pot'), stmap) in
                  let msg = Msg(mp, max(t,t')+1, (0,0), A.MLabI(mp, "nonempty", mpn)) in
                  let newproc = Proc(func, c, in_use, max(t,t')+1, (w, pot), p.A.st_structure) in
                  let config = add_sem newmap config in
                  let config = add_sem msg config in
                  let config = add_sem newproc config in
                  Changed config
                else
                  let config = remove_sem mp config in
                  let mpn = cfresh (mode_of mp) in
                  let mclose_proc = Proc("stmap", mpn, [], max(t,t')+1, (w', pot'), A.Close(mpn)) in
                  let msg = Msg(mp, max(t,t')+1, (0,0), A.MLabI(mp, emp_status stmap, mpn)) in
                  let newproc = Proc(func, c, in_use, max(t,t')+1, (w, pot), p.A.st_structure) in
                  let config = add_sem mclose_proc config in
                  let config = add_sem msg config in
                  let config = add_sem newproc config in
                  Changed config
            | Some(MapFProc(mp', in_use', t', (w',pot'), fmap)) ->
                if uneq_name mp mp'
                then raise ExecImpossible
                else
                  let config = remove_sem mp config in
                  let new_in_use = remove_chan mp in_use in
                  let newproc = Proc(func, c, new_in_use, max(t,t')+1, (w+w',pot+pot'), p.A.st_structure) in
                  let config = add_sem newproc config in
                  Changed config
            | _m -> raise ExecImpossible
        end
    | _s -> raise ExecImpossible ;;

(*
let makechan ch config =
  let s = find_sem ch config in
  let config = remove_sem ch config in
  match s with
      Proc(func,c,in_use,t,(w,pot),A.MakeChan(x,_,n,p)) ->
        let newch = (A.Hash, "ch" ^ string_of_int n, A.Shared) in
        let in_use' = add_chan newch in_use in
        let p = A.subst_aug newch x p in
        let newproc = Proc(func,c,in_use',t+1,(w,pot),p.A.st_structure) in
        let config = add_sem newproc config in
        Changed config
    | _s -> raise ExecImpossible;;
*)

let toString arg = match arg with
    A.FArg e -> PP.pp_fexp () 0 e
  | A.STArg c -> PP.pp_chan c;;

let generate_string p args = match p with
    A.Word(s) -> (s, args)
  | A.PNewline -> ("\n", args)
  | PInt | PBool | PStr | PAddr | PChan ->
      match args with
          [] -> raise ExecImpossible
        | arg::args' -> (toString arg, args');;

let rec get_printable_string l args =
  match l with
      [] -> ""
    | p::ps ->
        let (s, args') = generate_string p args in
        let tl = get_printable_string ps args' in
        s ^ tl;;

let print ch config =
  let s = find_sem ch config in
  let config = remove_sem ch config in
  match s with
      Proc(func,c,in_use,t,(w,pot),A.Print(l,args,p)) ->
        let pl = get_printable_string l args in
        let () = if !F.verbosity >= 0 then print_string pl in
        let config = add_print pl config in
        let () = flush_all () in
        let newproc = Proc(func,c,in_use,t+1,(w,pot),p.A.st_structure) in
        let config = add_sem newproc config in
        Changed config
    | _s -> raise ExecImpossible;;

let match_and_one_step env sem config =
  match sem with
      Proc(_func,c,_in_use,_t,_wp,p) ->
        begin
          match p with
              A.Fwd _ ->
                fwd c config
            | A.Spawn _ ->
                spawn env c config
            | A.ExpName _ ->
                expand env c config

            | A.Lab(c',_l,_p) ->
                if c = c'
                then ichoice_S c config
                else echoice_S c config
            
            | A.Case(c', _bs) ->
                if c = c'
                then echoice_R c config
                else ichoice_R c config
            
            | A.Send(c',_e,_p) ->
                if c = c'
                then tensor_S c config
                else lolli_S c config
            | A.Recv(c',_x,_p) ->
                if c = c'
                then lolli_R c config
                else tensor_R c config
            
            | A.Close _ ->
                one_S c config
            | A.Wait _ ->
                one_R c config

            | A.Work _ ->
                work c config
            | A.Deposit _ ->
                deposit c config
            
            | A.Pay(c',_pot,_p) ->
                if c = c'
                then paypot_S c config
                else getpot_S c config
            | A.Get(c',_pot,_p) ->
                if c = c'
                then getpot_R c config
                else paypot_R c config

            | A.Accept _ ->
                up c config
            | A.Detach _ ->
                down c config
            
            | A.Acquire _ -> Unchanged config
            | A.Release _ -> Unchanged config

            | A.SendF(c',_m,_p) ->
                if c = c'
                then product_S c config
                else arrow_S c config
            | A.RecvF(c',_y,_p) ->
                if c = c'
                then arrow_R c config
                else product_R c config
            
            | A.Let _ ->
                letS c config
            
            | A.IfS _ ->
                ifS c config

            | A.FMapCreate _ ->
                fmapcreate c config
            
            | A.STMapCreate _ ->
                stmapcreate c config 

            | A.FMapInsert _ ->
                fmapinsert c config

            | A.STMapInsert _ ->
                stmapinsert c config

            | A.FMapDelete _ ->
                fmapdelete c config

            | A.STMapDelete _ ->
                stmapdelete c config

            | A.MapClose _ ->
                mapclose c config
            
            | A.Abort ->
                Aborted
            
            | A.Print _ ->
                print c config
            
        end
    | Msg _ -> Unchanged config
    | MapFProc _ -> Unchanged config
    | MapSTProc _ -> Unchanged config;;

let rec pp_sems sems =
  match sems with
      [] -> ""
    | sem::sems' -> pp_sem sem ^ "\n" ^ pp_sems sems';;

(*
let rec pp_maps maps =
  match maps with
      [] -> "=======================================\n"
    | (c,c')::maps' -> PP.pp_chan c ^ " -> " ^ PP.pp_chan c' ^ "\n" ^ pp_maps maps';; 
*)

let pp_config config = pp_sems (get_sems config);;

let rec subconf chlist added config =
  match chlist with
      [] -> []
    | ch::chs ->
        let s = find_sem ch config in
        if (List.mem ch added)
        then subconf chs added config
        else
          match s with
              Proc(_f,_c,in_use,_t,_wp,_p) ->
                let sub_in = subconf in_use (ch::added) config in
                let sub_chs = subconf chs (ch::added) config in
                [s] @ sub_in @ sub_chs
            | MapFProc(_c,in_use,_t,_wp,_m) ->
                let sub_in = subconf in_use (ch::added) config in
                let sub_chs = subconf chs (ch::added) config in
                [s] @ sub_in @ sub_chs
            | MapSTProc(_c,in_use,_t,_wp,_m) ->
                let sub_in = subconf in_use (ch::added) config in
                let sub_chs = subconf chs (ch::added) config in
                [s] @ sub_in @ sub_chs
            | Msg _ ->
                let sub_chs = subconf chs (ch::added) config in
                [s] @ sub_chs;;

let add_wp wp1 wp2 =
  let (w1,p1) = wp1 in
  let (w2,p2) = wp2 in
  (w1+w2,p1+p2);;

let wp_sem sem =
  match sem with
      Proc(_f,_c,_in_use,_t,wp,_p) -> wp
    | MapFProc(_c,_in_use,_t,wp,_m) -> wp
    | MapSTProc(_c,_in_use,_t,wp,_m) -> wp
    | Msg(_c,_t,wp,_m) -> wp;;

let rec wp_sems sems =
  match sems with
      [] -> (0,0)
    | sem::sems' ->
        let wp = wp_sem sem in
        add_wp wp (wp_sems sems');;

let pp_wp (w,p) = "(" ^ string_of_int w ^ "," ^ string_of_int p ^ ")";;

let subconfig ch config =
  let sems = subconf [ch] [] config in
  (pp_sems sems, pp_wp (wp_sems sems));;

type config_outcome =
    Fail
  | Success of configuration;;

let check_existence config sem =
  match sem with
      Proc(_f, _c, in_use, _t, _wp, _p) ->
        let _ = List.map (fun c -> find_sem c config) (linear_chans in_use) in ()
    | Msg _ -> ()
    | MapFProc _ -> ()
    | MapSTProc _ -> ();;

let rec step env config =
  let sems = get_sems config in
  let () = if !F.verbosity > 1
          then print_string ((pp_config config) ^
                            "---------------------------------------\n") in
  (* check that all in_use linear channels actually exist *)
  let _ = List.map (check_existence config) sems in
  let config = iterate_and_one_step env sems config false in
  config

and iterate_and_one_step env sems config stepped =
  match sems with
      [] -> if stepped then step env config else Success config
    | sem::sems' ->
        let stepped_config = match_and_one_step env sem config in
        match stepped_config with
            Changed config -> iterate_and_one_step env sems' config true
          | Unchanged config -> iterate_and_one_step env sems' config stepped
          | Aborted -> Fail;;

let error m = EM.error EM.Runtime None m;;

type state =
  {
    energy : int;
    gamma : A.chan list;
    delta : A.chan list;
    config : configuration;
  }

let checked_diff chs1 chs2 =
  let diff = chans_diff chs1 chs2 in
  if List.length chs1 - List.length chs2 = List.length diff
  then Some diff
  else None

let checked_remove ch l =
  if not (List.exists (eq_name ch) l)
  then None
  else Some(List.filter (uneq_name ch) l)

let valid_final_proc c p m st delta' st' =
  match m with
      A.Shared ->
        begin
          match p with
              A.Accept _ -> { st' with gamma = c::st.gamma; delta = delta' }
            | _ -> error "shared process not blocking on accept"
        end
    | A.Pure -> { st' with delta = c::delta' }
    | A.Linear -> error "channel at mode L in final state"
    | A.Transaction -> error "transaction still running"
    | A.Unknown -> error "process mode Unknown during runtime"
    | A.MVar _ -> error "process mode MVar during runtime";;

let valid_final_fmap c m delta' st' =
  match m with
      A.Shared -> error "functional map at mode S found"
    | A.Pure -> { st' with delta = c::delta' }
    | A.Linear -> error "functional map at mode L found"
    | A.Transaction -> error "transaction still running"
    | A.Unknown -> error "process mode Unknown during runtime"
    | A.MVar _ -> error "process mode MVar during runtime";;

let valid_final_stmap c m st delta' st' =
  match m with
      A.Shared -> { st' with gamma = c::st.gamma ; delta = delta'}
    | A.Pure -> { st' with delta = c::delta' }
    | A.Linear -> error "map at mode L found in final state"
    | A.Transaction -> error "transaction still running"
    | A.Unknown -> error "process mode Unknown during runtime"
    | A.MVar _ -> error "process mode MVar during runtime";;

let check_and_add (top : Chan.t) (sem : sem) (st : state): state option =
  match sem with
    Proc(_, c, in_use, _t, (work, pot), p) ->
      let (_, _, m) = c in
      let st' = { st with energy = st.energy + work + pot } in
      begin
        match checked_diff st.delta in_use with
            None -> None
          | Some(delta') -> Some(valid_final_proc c p m st delta' st')
      end
  | MapFProc(c, in_use, _t, (work, pot), _map) ->
      let (_, _, m) = c in
      let st' = { st with energy = st.energy + work + pot } in
      begin
        match checked_diff st.delta in_use with
            None -> None
          | Some(delta') -> Some(valid_final_fmap c m delta' st')
      end
  | MapSTProc(c, in_use, _t, (work, pot), _map) ->
      let (_, _, m) = c in
      let st' = { st with energy = st.energy + work + pot } in
      begin
        match checked_diff st.delta in_use with
            None -> None
          | Some(delta') -> Some(valid_final_stmap c m st delta' st')
      end
  | Msg(c, _, _, msg) ->
      let (_, _, m) = c in
      match m with
          A.Transaction ->
            if eq_name c top
            then Some { st with config = remove_sem c st.config }
            else error "transaction message not for main transaction"
        | A.Pure -> (
            match msg with
              A.MLabI (c, _, cplus)
            | A.MLabE (cplus, _, c)
            | A.MSendT (c, _, cplus)
            | A.MSendL (cplus, _, c)
            | A.MPayP (c, _, cplus)
            | A.MPayG (cplus, _, c)
            | A.MSendP (c, _, cplus)
            | A.MSendA (cplus, _, c) ->
                Option.map
                  (fun delta' ->
                     { st with delta = add_chan c delta' })
                  (checked_remove cplus st.delta)
            | A.MClose c ->
                Some { st with delta = add_chan c st.delta })
        | _ -> error "dangling message"

(* verify the configuration after a transaction has executed *)
let verify_final_configuration top config =
  let step_state st =
    List.fold_left (fun (st, sems, changed) sem ->
      match check_and_add top sem st with
          None -> (st, sem::sems, changed)
        | Some st' -> (st', sems, true))
      (st, [], false) in
  let rec st_fixpoint st sems =
    let (st', sems', changed) = step_state st sems in
    if changed then st_fixpoint st' sems' else (st', sems') in
  let sems0 = get_sems config in
  let st0 = { energy = 0; delta = []; gamma = []; config } in
  let (st_final, sems_final) = st_fixpoint st0 sems0 in
  let config' = st_final.config in
  if List.length sems_final > 0
    then error "could not add some sems to final configuration"
    else config'

(* transaction num * channel num * gas accounts * environment * configuration *)
type blockchain_state = int * int * G.gas_accounts * A.decl list * configuration
[@@deriving sexp]

let leftover_gas () = !txnGas;;

let empty_blockchain_state =
  (0,
   0,
   M.empty (module C.String),
   [],
   {
     conf = M.empty (module Chan);
     conts = M.empty (module Chan);
     shared = M.empty (module Chan);
     types = M.empty (module Chan);
     print_state = ""
   });;

let rec arg_chans args =
  match args with
      [] -> []
    | (A.FArg _)::args' -> arg_chans args'
    | (A.STArg(c))::args' -> c::(arg_chans args');;

let try_exec f c pot args initial_config env types =
  try
    begin
      let sem = Proc(f,c,arg_chans args,0,(0,pot),A.ExpName(c,f,args)) in
      let config = add_sem sem initial_config in
      match step env config with
          Fail -> (!txnNum + 1, !chan_num, types, initial_config)
        | Success final_config ->
            (!txnNum + 1, !chan_num, types, verify_final_configuration c final_config)          
    end
  with
    | InsufficientPotential -> error "insufficient potential during execution"
    | UnconsumedPotential -> error "unconsumed potential during execution"
    | PotentialMismatch -> error "potential mismatch during execution"
    | MissingBranch -> error "missing branch during execution"
    | ChannelMismatch -> error "channel name mismatch found at runtime"
    | UndefinedProcess -> error "undefined process found at runtime"
    | StarPotential -> error "potential * found at runtime";;

(* exec env C state (f, args) = C'
 * env is the elaborated environment
 * C is a full configuration
 * C' is final, poised configuration
 *)
let exec env state (f, args) =
  let (tx, ch, gas_accs, types, initial_config) = state in
  let () = txnNum := tx in
  let () = chan_num := ch in
  let m = chan_mode env f in
  let c = cfresh m in
  let pot = try_evaluate (A.get_pot env f) in
  let (res, deducted_gas_accs) = G.deduct gas_accs !txnSender pot in
  let () = txnGas := 0 in
  match res with
      Insufficient bal ->
        let () = if !F.verbosity >= 0 then print_string ("% txn sender " ^ !txnSender ^ " does not have sufficient gas, needed: " ^ string_of_int pot ^ ", found:  " ^ string_of_int bal ^ "\n") in
        (!txnNum + 1, !chan_num, gas_accs, types, initial_config)
    | NonBC ->
        let () = if !F.verbosity >= 0 then print_string ("% running program in non-blockchain mode\n") in
        let (tx, ch, types, config) = try_exec f c pot args initial_config env types in
        (tx, ch, deducted_gas_accs, types, config)
    | Balance bal ->
        let () = if !F.verbosity >= 0 then print_string ("% gas cost of txn: " ^ string_of_int pot ^ " units successfully deducted; txn sender " ^ !txnSender ^ " now has " ^ string_of_int bal ^ " gas units\n") in
        let (tx, ch, types, config) = try_exec f c pot args initial_config env types in
        let () = if !F.verbosity >= 0 then print_string ("% depositing leftover gas\n") in
        let deposited_gas_accs = G.deposit deducted_gas_accs !txnSender !txnGas in
        (tx, ch, deposited_gas_accs, types, config);;
