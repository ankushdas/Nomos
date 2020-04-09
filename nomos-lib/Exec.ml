module C = Core
module R = Arith
module A = Ast
module PP = Pprint
module M = C.Map
module F = NomosFlags

exception InsufficientPotential (* not enough potential to work or pay *)

exception UnconsumedPotential (* too much potential during termination *)

exception PotentialMismatch (* mismatch while spawning or exchanging *)

exception MissingBranch (* missing branch during internal/external choice *)

exception ProgressError (* final configuration inconsistent *)

exception ChannelMismatch (* channel name mismatch in sem *)

exception UndefinedProcess (* spawning an undefined process *)

exception ExecImpossible (* should never happen at runtime *)

exception StarPotential (* star potential encountered at runtime *)

exception RuntimeError (* should never happen at runtime *)

open Sexplib.Std
type sem =
    (* Proc(chan, in_use, time, (work, pot), P) *)
    Proc of string * A.chan * A.chan list * int * (int * int) * A.ext A.st_expr
    (* Msg(chan, time, (work, pot), M) *)
  | Msg of A.chan * int * (int * int) * A.ext A.msg
  [@@deriving sexp]

let pp_sem sem = match sem with
    Proc(f,c,in_use,t,(w,pot),p) ->
      f ^ ": proc(" ^ PP.pp_chan c ^ ", in_use = [" ^ PP.pp_channames in_use ^
      "], t = " ^ string_of_int t ^ ", (w = " ^ string_of_int w ^
      ", pot = " ^ string_of_int pot ^ "), " ^ PP.pp_exp_prefix p ^ ")"
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

let rec eval fexp = match fexp.A.func_structure with
    A.If(e1,e2,e3) ->
      begin
        let (v1, c1) = eval e1 in
        match v1 with
            A.BoolV true ->
              let (v2, c2) = eval e2 in
              (v2, R.plus c1 c2)
          | A.BoolV false ->
              let (v3, c3) = eval e3 in
              (v3, R.plus c1 c3)
          | _ -> raise RuntimeError
      end
  | A.LetIn(x,e1,e2) ->
      begin
        let (v1, c1) = eval e1 in
        let e2 = A.substv_aug (A.toExpr e1.func_data v1) x e2 in
        let (v2, c2) = eval e2 in
        (v2, R.plus c1 c2)
      end
  | A.Bool b -> (A.BoolV b, R.Int 0)
  | A.Int i -> (A.IntV i, R.Int 0)
  | A.Addr a -> (A.AddrV a, R.Int 0)
  | A.Var x -> raise RuntimeError
  | A.ListE(l) ->
      begin
        let (vs, cs) = proj (List.map eval l) in
        (A.ListV vs, List.fold_left (fun s x -> R.plus s x) (R.Int 0) cs)
      end
  | A.App(l) ->
      begin
        let (l', en) = A.split_last l in
        let (v', c') = eval {A.func_structure = A.App(l') ; A.func_data = None} in
        let (vn, cn) = eval en in
        match v' with
            A.LambdaV (xs, e) ->
              begin
                match xs with
                    A.Single (x,_ext) ->
                      let (v, c) = eval (A.substv_aug (A.toExpr en.func_data vn) x e) in
                      (v, R.plus c (R.plus c' cn))
                  | A.Curry ((x,_ext),xs') -> (A.LambdaV(xs', A.substv_aug (A.toExpr en.func_data vn) x e), R.plus c' cn)
              end
          | _ -> raise RuntimeError
      end
  | A.Cons(e1,e2) ->
      begin
        let (v1, c1) = eval e1 in
        let (v2, c2) = eval e2 in
        match v2 with
            A.ListV l -> (A.ListV(v1::l), R.plus c1 c2)
          | _ -> raise RuntimeError
      end
  | A.Match(e1,e2,x,xs,e3) ->
      begin
        let (v1, c1) = eval e1 in
        match v1 with
            A.ListV l ->
              begin
                match l with
                    [] ->
                      let (v2, c2) = eval e2 in
                      (v2, R.plus c1 c2)
                  | v::vs ->
                      let e3 = A.substv_aug (A.toExpr None v) x e3 in
                      let e3 = A.substv_aug (A.toExpr None (A.ListV vs)) xs e3 in
                      let (v3, c3) = eval e3 in
                      (v3, R.plus c1 c3)
              end
          | _ -> raise RuntimeError
      end
  | A.Lambda(xs,e) -> (A.LambdaV(xs,e), R.Int 0)
  | A.Op(e1,op,e2) ->
      begin
        let (v1, c1) = eval e1 in
        let (v2, c2) = eval e2 in
        match v1, v2 with
            A.IntV i1, A.IntV i2 -> (A.IntV (apply_op op i1 i2), R.plus c1 c2)
          | _, _ -> raise RuntimeError
      end
  | A.CompOp(e1,cop,e2) ->
      begin
        let (v1, c1) = eval e1 in
        let (v2, c2) = eval e2 in
        match v1, v2 with
            A.IntV i1, A.IntV i2 -> (A.BoolV (compare_op cop i1 i2), R.plus c1 c2)
          | _, _ -> raise RuntimeError
      end
  | A.EqAddr(e1,e2) ->
      begin
        let (v1, c1) = eval e1 in
        let (v2, c2) = eval e2 in
        match v1, v2 with
            A.AddrV a1, A.AddrV a2 -> (A.BoolV (a1 = a2), R.plus c1 c2)
          | _, _ -> raise RuntimeError
      end
  | A.RelOp(e1,rop,e2) ->
      begin
        let (v1, c1) = eval e1 in
        let (v2, c2) = eval e2 in
        match v1, v2 with
            A.BoolV b1, A.BoolV b2 -> (A.BoolV (relate_op rop b1 b2), R.plus c1 c2)
          | _, _ -> raise RuntimeError
      end
  | A.Tick(pot,e) ->
      begin
        let (v, c) = eval e in
        match pot with
            A.Star -> raise RuntimeError
          | A.Arith p -> (v, R.plus c p)
      end
  | A.GetTxnNum -> (A.IntV !txnNum, R.Int 0)
  | A.GetTxnSender -> (A.AddrV !txnSender, R.Int 0)
  | A.Command(p) -> raise RuntimeError;;


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
  } [@@deriving sexp]

type stepped_config =
    Changed of configuration
  | Unchanged of configuration;;

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
    | Some v -> v

type pol = Pos | Neg

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
                  | Some (Msg _) -> sem_obj
        end
    | Pos ->
        let sem_obj = M.find conf c in
        match sem_obj with
            None -> None
          | Some (Proc _) -> None
          | Some (Msg _) -> sem_obj;;

let remove_sem c config =
  { config with conf = M.remove config.conf c }

let add_sem sem config =
  match sem with
      Proc(_f,c,_in_use,_t,_wp,_p) ->
        { config with conf = M.add_exn config.conf ~key:c ~data:sem }
    | Msg(c,_t,_wp,_p) ->
        { config with conf = M.add_exn config.conf ~key:c ~data:sem }

let add_shared_map (c,c') config =
  { config with shared = M.add_exn config.shared ~key:c ~data:c' }

let get_shared_chan c config =
  match M.find config.shared c with
      None -> raise ExecImpossible
    | Some c' -> c'

let remove_shared_map c config =
  { config with shared = M.remove config.shared c }

let add_cont (c,c') config =
  match M.find config.shared c with
      None -> { config with conts = M.add_exn config.conts ~key:c ~data:c' }
    | Some cs ->
        let config = remove_shared_map c config in
        let config = add_shared_map (c',cs) config in
        { config with conts = M.add_exn config.conts ~key:c ~data:c' }

let remove_cont c config =
  { config with conts = M.remove config.conts c }

let get_cont c config =
  match M.find config.conts c with
      None -> raise ExecImpossible
    | Some c' -> c'

let add_chan_tp c t config =
  { config with types = M.add_exn config.types ~key:c ~data:t }

let get_pot env f =
  match A.lookup_expdec env f with
      None -> raise UndefinedProcess
    | Some(_ctx,pot,_zc,_m) -> pot;;

let eq_name (_s1,c1,_m1) (_s2,c2,_m2) = c1 = c2;;

let uneq_name c1 c2 = not (eq_name c1 c2);;

let mode_of (_s,_c,m) = m;;

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
    | Some(_ctx,_pot,_zc,m) -> m

let chan_tp env f =
  match A.lookup_expdec env f with
      None -> raise UndefinedProcess
    | Some(_ctx,_pot,zc,_m) -> let (_c, t) = zc in t 

let add_chan ch in_use = ch::in_use

let replace_chan ch' ch l =
  if not (List.exists (eq_name ch) l)
  then raise ExecImpossible
  else
    List.map
      (fun other -> if eq_name ch other then ch' else other)
      l

let remove_chan ch l =
  if not (List.exists (eq_name ch) l)
  then raise ExecImpossible
  else List.filter (uneq_name ch) l

let chans_diff chs1 chs2 =
  List.filter
    (fun x -> not (List.exists (eq_name x) chs2))
    chs1

let is_linear (_, _, m) =
  match m with
    A.Linear | A.Pure -> true
  | _ -> false

let extract_chans l =
  List.filter_map
    (fun arg ->
       match arg with
         A.STArg c -> Some c
       | A.FArg _ -> None) l |>
  List.partition is_linear

let linear_chans = List.filter is_linear

let spawn env ch config =
  let s = find_sem ch config in
  let config = remove_sem ch config in
  match s with
      Proc(func,d,in_use,t,(w,pot),A.Spawn(x,f,xs,q)) ->
        let m = chan_mode env f in
        let c' = cfresh m in
        let pot' = try_evaluate (get_pot env f) in
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
        let pot' = try_evaluate (get_pot env f) in
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
      Proc(func,c1,in_use,t,(w,pot),A.Close(c2)) ->
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
      Proc(func,d,in_use,t,(w,pot),A.Get(c,epot,q)) as f ->
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
    | (Proc(func,_c,_in_use,_t,_wp,A.Acquire(a,_x,_p)) as proc)::sems' ->
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
    | l -> Some(List.nth l 0);;

let up ch config =
  let s = find_sem ch config in
  match s with
      Proc(func_acc,as1,in_use,t,wp,A.Accept(as2,x,p)) ->
        begin
          if uneq_name as1 as2
          then raise ChannelMismatch
          else
            let procs = find_acquiring_procs as1 config in
            let proc_opt = pick_random procs in
            match proc_opt with
                None -> Unchanged config
              | Some proc ->
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
    | (Proc(func,_c,_in_use,_t,_wp,A.Release(a,_x,_p)) as proc)::sems' ->
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
          let (v, acost) = eval e in
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
          let (v, acost) = eval e in
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
        let (v, acost) = eval e in
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
        let (v, acost) = eval e in
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

let makechan ch config =
  let s = find_sem ch config in
  let config = remove_sem ch config in
  match s with
      Proc(func,c,in_use,t,(w,pot),A.MakeChan(x,a,n,p)) ->
        let newch = (A.Hash, "ch" ^ string_of_int n, A.Shared) in
        let in_use' = add_chan newch in_use in
        let p = A.subst_aug newch x p in
        let newproc = Proc(func,c,in_use',t+1,(w,pot),p.A.st_structure) in
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
            
            | A.MakeChan _ ->
                makechan c config
            
        end
    | Msg _ -> Unchanged config;;

let rec pp_sems sems =
  match sems with
      [] -> "---------------------------------------\n"
    | sem::sems' -> pp_sem sem ^ "\n" ^ pp_sems sems';;

let rec pp_maps maps =
  match maps with
      [] -> "=======================================\n"
    | (c,c')::maps' -> PP.pp_chan c ^ " -> " ^ PP.pp_chan c' ^ "\n" ^ pp_maps maps';; 

let pp_config config = pp_sems (get_sems config)

let rec step env config =
  let sems = get_sems config in
  let () = print_string (pp_config config) in
  (* check that all in_use linear channels actually exist *)
  let _ = List.map (fun sem ->
    match sem with
      (Proc(_f, _c, in_use, _t, _wp, _p)) ->
        let _ = List.map (fun c -> find_sem c config) (linear_chans in_use) in ()
    | (Msg(c, t, wp, msg)) -> ()) sems in
  let config = iterate_and_one_step env sems config false in
  config

and iterate_and_one_step env sems config stepped =
  match sems with
      [] -> if stepped then step env config else config
    | sem::sems' ->
        let stepped_config = match_and_one_step env sem config in
        match stepped_config with
            Changed config -> iterate_and_one_step env sems' config true
          | Unchanged config -> iterate_and_one_step env sems' config stepped;;

let error = ErrorMsg.error_msg ErrorMsg.Runtime None;;

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

let check_and_add (top : Chan.t) (sem : sem) (st : state): state option =
  match sem with
    (Proc(f, c, in_use, _t, (work, pot), p)) -> (
      let (_, _, m) = c in
      let st' = { st with energy = st.energy + work + pot } in
      match checked_diff st.delta in_use with
        None -> None
      | Some(delta') -> Some(
        match m with
          | A.Shared -> (
              match p with
                  A.Accept _ ->
                    { st' with gamma = c::st.gamma; delta = delta' }
                | _ -> error "shared process not blocking on accept";
                       raise RuntimeError)
          | A.Pure -> { st' with delta = c::delta' }
          | A.Linear -> (error "linear channel in final state";
                         raise RuntimeError)
          | A.Transaction -> (error "transaction still running";
                              raise RuntimeError)
          | A.Unknown -> (error "process mode Unknown during runtime";
                          raise RuntimeError)
          | A.MVar _ -> (error "process mode MVar during runtime";
                         raise RuntimeError)))
  | (Msg(c, t, wp, msg)) ->
      let (_, _, m) = c in
      match m with
          A.Transaction ->
            if eq_name c top
            then Some { st with config = remove_sem c st.config }
            else (error "transaction message not for main transaction";
                  raise RuntimeError)
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
        | _ -> error "dangling message";
               raise RuntimeError

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
    then (error "could not add some sems to final configuration";
          raise RuntimeError)
    else config'

type type_map = A.stype M.M(C.String).t [@@deriving sexp]

(* transaction num * channel num * type names * configuration *)
type full_configuration = int * int * type_map * configuration
[@@deriving sexp]

let empty_full_configuration =
  (0, 0, M.empty (module C.String), {
    conf = M.empty (module Chan);
    conts = M.empty (module Chan);
    shared = M.empty (module Chan);
    types = M.empty (module Chan);
  })

(* exec env C (f, args) = C'
 * env is the elaborated environment
 * C is a full configuration
 * C' is final, poised configuration
 *)
let exec env (full_config : full_configuration) (f, args) =
  let (tx, ch, types, initial_config) = full_config in
  let () = txnNum := tx in
  let () = chan_num := ch in
  let m = chan_mode env f in
  let c = cfresh m in
  let pot = try_evaluate (get_pot env f) in
  let sem = Proc(f,c,[],0,(0,pot),A.ExpName(c,f,args)) in
  let config = add_sem sem initial_config in
  try (!txnNum + 1, !chan_num, types, verify_final_configuration c (step env config))
  with
    | InsufficientPotential -> error "insufficient potential during execution"
                               ; raise RuntimeError
    | UnconsumedPotential -> error "unconsumed potential during execution"
                             ; raise RuntimeError
    | PotentialMismatch -> error "potential mismatch during execution"
                           ; raise RuntimeError
    | MissingBranch -> error "missing branch during execution"
                       ; raise RuntimeError
    | ProgressError -> error "final configuration inconsistent"
                       ; raise RuntimeError
    | ChannelMismatch -> error "channel name mismatch found at runtime"
                         ; raise RuntimeError
    | UndefinedProcess -> error "undefined process found at runtime"
                          ; raise RuntimeError
    | StarPotential -> error "potential * found at runtime"
                       ; raise RuntimeError;;
