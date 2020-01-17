module R = Arith
module A = Ast
module PP = Pprint
module C = Core
module M = C.Map

exception InsufficientPotential (* not enough potential to work or pay *)

exception UnconsumedPotential (* too much potential during termination *)

exception PotentialMismatch (* mismatch while spawning or exchanging *)

exception MissingBranch (* missing branch during internal/external choice *)

exception MarkedExpCategory (* marked expression found at runtime *)

exception ProgressError (* final configuration inconsistent *)

exception ChannelMismatch (* channel name mismatch in sem *)

exception UndefinedProcess (* spawning an undefined process *)

exception ExecImpossible (* should never happen at runtime *)

exception StarPotential (* star potential encountered at runtime *)

exception RuntimeError (* should never happen at runtime *)

type sem =
    Proc of A.chan * int * (int * int) * A.ext A.st_expr          (* Proc(chan, time, (work, pot), P) *)
  | Msg of A.chan * int * (int * int) * A.msg                     (* Msg(chan, time, (work, pot), M) *)

let pp_sem sem = match sem with
    Proc(c,t,(w,pot),p) ->
      "proc(" ^ PP.pp_chan c ^ ", t = " ^ string_of_int t ^ ", (w = " ^ string_of_int w ^
      ", pot = " ^ string_of_int pot ^ "), " ^ PP.pp_exp_prefix p ^ ")"
  | Msg(c,t,(w,pot),m) ->
      "msg(" ^ PP.pp_chan c ^ ", t = " ^ string_of_int t ^ ", (w = " ^ string_of_int w ^
      ", pot = " ^ string_of_int pot ^ "), " ^ PP.pp_msg m ^ ")";;

module Chan =
  struct
    module T =
      struct
        type t = A.chan

        let compare x y =
          let (c1,_m1) = x in
          let (c2,_m2) = y in
          C.String.compare c1 c2
        
        let sexp_of_t (c,_m) = C.String.sexp_of_t c

        let t_of_sexp s = (C.String.t_of_sexp s, A.Unknown)
      end
      include T
      include C.Comparable.Make(T)
  end

(* map from offered channel to semantic object *)
type map_chan_sem = sem Chan.Map.t;;

(* map from channel to its continuation *)
type map_chan_chan = A.chan Chan.Map.t;;

(* configuration type *)
(* map from offered channel to semantic object *)
(* map from channel to its continuation channel *)
(* map from linear channel to its shared counterpart *)
type configuration = map_chan_sem * map_chan_chan * map_chan_chan;;

type stepped_config =
    Changed of configuration
  | Unchanged of configuration;;

let chan_num = ref 0;;

let lfresh () =
  let n = !chan_num in
  let () = chan_num := n+1 in
  (A.Dollar, "ch" ^ (string_of_int n), A.Unknown);;

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

let find_sem c (conf,_conts,_shared) =
  match M.find conf c with
      None -> raise ExecImpossible
    | Some v -> v;;

type pol = Pos | Neg;;

let find_msg c (conf,conts,_shared) dual =
  match dual with
      Neg ->
        begin
          match M.find conts c with
              None -> None
            | Some c' -> M.find conf c'
        end
    | Pos -> M.find conf c;;

let remove_sem c (conf,conts,shared) =
  (M.remove conf c, conts, shared);;

let add_sem sem (conf,conts,shared) =
  match sem with
      Proc(c,_t,_wp,_p) ->
        (M.add_exn conf ~key:c ~data:sem, conts, shared)
    | Msg(c,_t,_wp,_p) ->
        (M.add_exn conf ~key:c ~data:sem, conts, shared);;

let add_shared_map (c,c') (conf,conts,shared) =
  (conf, conts, M.add_exn shared ~key:c ~data:c');;

let get_shared_chan c (_conf,_conts,shared) =
  match M.find shared c with
      None -> raise ExecImpossible
    | Some c' -> c';;

let remove_shared_map c (conf,conts,shared) =
  (conf, conts, M.remove shared c);;
        
let add_cont (c,c') (conf,conts,shared) =
  match M.find shared c with
      None -> (conf, M.add_exn conts ~key:c ~data:c', shared)
    | Some cs ->
        let config = (conf,conts,shared) in
        let config = remove_shared_map c config in
        let config = add_shared_map (c',cs) config in
        let (conf,conts,shared) = config in
        (conf, M.add_exn conts ~key:c ~data:c', shared);;

let remove_cont c (conf,conts,shared) =
  (conf, M.remove conts c,shared);;

let get_cont c (_conf,conts,_shared) =
  match M.find conts c with
      None -> raise ExecImpossible
    | Some c' -> c';;

let get_pot env f =
  match A.lookup_expdec env f with
      None -> raise UndefinedProcess
    | Some(_ctx,pot,_zc,_m) -> pot;;

let uneq_name (_s1,c1,_m1) (_s2,c2,_m2) = not (c1 = c2);;

let fwd ch config =
  let s = find_sem ch config in
  match s with
      Proc(c1,t,(w,pot),A.Fwd(c2,d)) ->
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
                      | Some(Msg(_d,t',(w',pot'),(A.MLabI _ as m)))
                      | Some(Msg(_d,t',(w',pot'),(A.MSendL _ as m)))
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
              | Some(Msg(_d,t',(w',pot'),(A.MClose _ as m)))
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
              | _s -> Unchanged config
          end
    | _s -> raise ExecImpossible;;

let spawn env ch config =
  let s = find_sem ch config in
  let config = remove_sem ch config in
  match s with
      Proc(d,t,(w,pot),A.Spawn(x,f,xs,q)) ->
        let c' = lfresh () in
        let pot' = try_evaluate (get_pot env f) in
        if pot < pot'
        then raise InsufficientPotential
        else
          let proc1 = Proc(c',t+1,(0,pot'),A.ExpName(c',f,xs)) in
          let proc2 = Proc(d,t+1,(w,pot-pot'),A.subst c' x q.A.st_structure) in
          let config = add_sem proc1 config in
          let config = add_sem proc2 config in
          Changed config
    | _s -> raise ExecImpossible;;

let rec fst l = match l with
    (c,_t)::l' -> c::(fst l')
  | [] -> [];;

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
      Proc(c,t,(w,pot),A.ExpName(x,f,xs)) ->
        let p = expd_def env x f xs in
        let pot' = try_evaluate (get_pot env f) in
        if pot <> pot'
        then raise PotentialMismatch
        else
          let proc = Proc(c,t,(w,pot),A.subst c x p.A.st_structure) in
          let config = add_sem proc config in
          Changed config
    | _s -> raise ExecImpossible;;

let ichoice_S ch config =
  let s = find_sem ch config in
  let config = remove_sem ch config in
  match s with
      Proc(c1,t,wp,A.Lab(c2,l,p)) ->
        if uneq_name c1 c2
        then raise ChannelMismatch
        else
          let c' = lfresh () in
          let msg = Msg(c1,t+1,(0,0),A.MLabI(c1,l,c')) in
          let proc = Proc(c',t+1,wp,A.subst c' c1 p) in
          let config = add_sem msg config in
          let config = add_sem proc config in
          let config = add_cont (c1,c') config in
          Changed config
    | _s -> raise ExecImpossible;;

let ichoice_R ch config =
  let s = find_sem ch config in
  match s with
      Proc(d,t,(w,pot),A.Case(c,bs)) ->
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
                    let proc = Proc(d, max(t,t')+1, (w+w',pot+pot'), A.subst c' c q) in
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
      Proc(d,t,wp,A.Lab(c,l,p)) ->
        if not (uneq_name d c)
        then raise ChannelMismatch
        else
          let c' = lfresh () in
          let msg = Msg(c',t+1,(0,0),A.MLabE(c,l,c')) in
          let proc = Proc(d,t+1,wp,A.subst c' c p) in
          let config = add_sem msg config in
          let config = add_sem proc config in
          let config = add_cont (c,c') config in
          Changed config
    | _s -> raise ExecImpossible;;

let echoice_R ch config =
  let s = find_sem ch config in
  match s with
      Proc(c1,t,(w,pot),A.Case(c2,bs)) ->
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
                    let proc = Proc(c2', max(t,t')+1, (w+w',pot+pot'), A.subst c2' c2 q) in
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
      Proc(c1,t,wp,A.Send(c2,e,p)) ->
        if uneq_name c1 c2
        then raise ChannelMismatch
        else
          let c' = lfresh () in
          let msg = Msg(c1,t+1,(0,0),A.MSendT(c1,e,c')) in
          let proc = Proc(c',t+1,wp,A.subst c' c1 p) in
          let config = add_sem msg config in
          let config = add_sem proc config in
          let config = add_cont (c1,c') config in
          Changed config
    | _s -> raise ExecImpossible;;

let tensor_R ch config =
  let s = find_sem ch config in
  match s with
      Proc(d,t,(w,pot),A.Recv(c,x,q)) ->
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
                    let q = A.subst e x q in
                    let proc = Proc(d, max(t,t')+1, (w+w',pot+pot'), A.subst c' c q) in
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
      Proc(d,t,wp,A.Send(c,e,p)) ->
        if not (uneq_name d c)
        then raise ChannelMismatch
        else
          let c' = lfresh () in
          let msg = Msg(c',t+1,(0,0),A.MSendL(c,e,c')) in
          let proc = Proc(d,t+1,wp,A.subst c' c p) in
          let config = add_sem msg config in
          let config = add_sem proc config in
          let config = add_cont (c,c') config in
          Changed config
    | _s -> raise ExecImpossible;;

let lolli_R ch config =
  let s = find_sem ch config in
  match s with
      Proc(c1,t,(w,pot),A.Recv(c2,x,q)) ->
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
                    let q = A.subst e x q in
                    let proc = Proc(c2', max(t,t')+1, (w+w',pot+pot'), A.subst c2' c2 q) in
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
      Proc(c1,t,(w,pot),A.Close(c2)) ->
        if uneq_name c1 c2
        then raise ChannelMismatch
        else if pot > 0
        then raise UnconsumedPotential
        else
          let msg = Msg(c1,t+1,(w,pot),A.MClose(c1)) in
          let config = add_sem msg config in
          Changed config
    | _s -> raise ExecImpossible;;

let one_R ch config =
  let s = find_sem ch config in
  match s with
      Proc(d,t,(w,pot),A.Wait(c,q)) ->
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
                    let proc = Proc(d, max(t,t')+1, (w+w',pot+pot'), q) in
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
      Proc(c,t,(w,pot),A.Work(k,p)) ->
        let k = try_evaluate k in
        if pot < k
        then raise InsufficientPotential
        else
          let proc = Proc(c,t+1,(w+k,pot-k),p) in
          let config = add_sem proc config in
          Changed config
    | _s -> raise ExecImpossible;;

let paypot_S ch config =
  let s = find_sem ch config in
  let config = remove_sem ch config in
  match s with
      Proc(c1,t,(w,pot),A.Pay(c2,epot,p)) ->
        if uneq_name c1 c2
        then raise ChannelMismatch
        else if pot < try_evaluate epot
        then raise InsufficientPotential
        else
          let c' = lfresh () in
          let vpot = try_evaluate epot in
          let msg = Msg(c1,t+1,(0,vpot),A.MPayP(c1,epot,c')) in
          let proc = Proc(c',t+1,(w,pot-vpot),A.subst c' c1 p) in
          let config = add_sem msg config in
          let config = add_sem proc config in
          let config = add_cont (c1,c') config in
          Changed config
    | _s -> raise ExecImpossible;;

let paypot_R ch config =
  let s = find_sem ch config in
  match s with
      Proc(d,t,(w,pot),A.Get(c,epot,q)) ->
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
                    let proc = Proc(d, max(t,t')+1, (w+w',pot+pot'), A.subst c' c q) in
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
      Proc(d,t,(w,pot),A.Pay(c,epot,p)) ->
        if not (uneq_name d c)
        then raise ChannelMismatch
        else if pot < try_evaluate epot
        then raise InsufficientPotential
        else
          let c' = lfresh () in
          let vpot = try_evaluate epot in
          let msg = Msg(c',t+1,(0,vpot),A.MPayG(c,epot,c')) in
          let proc = Proc(d,t+1,(w,pot-vpot),A.subst c' c p) in
          let config = add_sem msg config in
          let config = add_sem proc config in
          let config = add_cont (c,c') config in
          Changed config
    | _s -> raise ExecImpossible;;

let getpot_R ch config =
  let s = find_sem ch config in
  match s with
      Proc(c1,t,(w,pot),A.Get(c2,epot,q)) ->
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
                    let proc = Proc(c2', max(t,t')+1, (w+w',pot+pot'), A.subst c2' c2 q) in
                    let config = remove_sem ch config in
                    let config = remove_sem c2' config in
                    let config = add_sem proc config in
                    let config = remove_cont c2 config in
                    Changed config
              | _m -> Unchanged config
          end
    | _s -> raise ExecImpossible;;

let get_sems (conf,_conts,_shared) =
  M.fold_right conf ~init:[] ~f:(fun ~key:_k ~data:v l -> v::l);;

let rec find_procs ch sems =
  match sems with
      [] -> []
    | (Proc(_c,_t,_wp,A.Acquire(a,_x,_p)) as proc)::sems' ->
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
    | l -> Some(List.nth l (Random.int (List.length l)));;

let up ch config =
  let s = find_sem ch config in
  match s with
      Proc(as1,t,wp,A.Accept(as2,x,p)) ->
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
                      Proc(c,t',wp',A.Acquire(aseq,x',q)) ->
                        if uneq_name aseq as1
                        then raise ChannelMismatch
                        else
                          let al = lfresh () in
                          let proc1 = Proc(al,max(t,t')+1,wp,A.subst al x p) in
                          let proc2 = Proc(c,max(t,t')+1,wp',A.subst al x' q) in
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
    | (Proc(_c,_t,_wp,A.Release(a,_x,_p)) as proc)::sems' ->
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
      Proc(al1,t,wp,A.Detach(al2,x,p)) ->
        begin
          if uneq_name al1 al2
          then raise ChannelMismatch
          else
            let proc = find_releasing_proc al1 config in
            match proc with
                None -> Unchanged config
              | Some proc ->
                  match proc with
                      Proc(c,t',wp',A.Release(aleq,x',q)) ->
                        if uneq_name aleq al1
                        then raise ChannelMismatch
                        else
                          let ash = get_shared_chan al1 config in
                          let proc1 = Proc(ash,max(t,t')+1,wp,A.subst ash x p) in
                          let proc2 = Proc(c,max(t,t')+1,wp',A.subst ash x' q) in
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
      Proc(c1,t,wp,A.SendF(c2,m,p)) ->
        if uneq_name c1 c2
        then raise ChannelMismatch
        else
          let c' = lfresh () in
          let v = eval m in
          let msg = Msg(c1,t+1,(0,0),A.MSendP(c1,v,c')) in
          let proc = Proc(c',t+1,wp,A.subst c' c1 p) in
          let config = add_sem msg config in
          let config = add_sem proc config in
          let config = add_cont (c1,c') config in
          Changed config
    | _s -> raise ExecImpossible;;
    
let product_R ch config =
  let s = find_sem ch config in
  match s with
      Proc(d,t,(w,pot),A.RecvF(c,x,q)) ->
        if not (uneq_name d c)
        then raise ChannelMismatch
        else
          let msg = find_msg c config Pos in
          begin
            match msg with
                Some(Msg(ceq, t', (w',pot'), A.MSendP(_ceq,e,c'))) ->
                  if uneq_name ceq c
                  then raise ChannelMismatch
                  else
                    let q = A.subst e x q in
                    let proc = Proc(d, max(t,t')+1, (w+w',pot+pot'), A.subst c' c q) in
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
          Proc(d,t,wp,A.Send(c,e,p)) ->
            if not (uneq_name d c)
            then raise ChannelMismatch
            else
              let c' = lfresh () in
              let msg = Msg(c',t+1,(0,0),A.MSendL(c,e,c')) in
              let proc = Proc(d,t+1,wp,A.subst c' c p) in
              let config = add_sem msg config in
              let config = add_sem proc config in
              let config = add_cont (c,c') config in
              Changed config
        | _s -> raise ExecImpossible;;
    
    let lolli_R ch config =
      let s = find_sem ch config in
      match s with
          Proc(c1,t,(w,pot),A.Recv(c2,x,q)) ->
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
                        let q = A.subst e x q in
                        let proc = Proc(c2', max(t,t')+1, (w+w',pot+pot'), A.subst c2' c2 q) in
                        let config = remove_sem ch config in
                        let config = remove_sem c2' config in
                        let config = add_sem proc config in
                        let config = remove_cont c2 config in
                        Changed config
                  | _m -> Unchanged config
              end
        | _s -> raise ExecImpossible;;
    

let match_and_one_step env sem config =
  match sem with
      Proc(c,_t,_wp,p) ->
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
            
            | A.Marked _ ->
                raise MarkedExpCategory
        end
    | Msg _ -> Unchanged config;;

let rec pp_sems sems =
  match sems with
      [] -> "---------------------------------------\n"
    | sem::sems' -> pp_sem sem ^ "\n" ^ pp_sems sems';;

let get_maps (_conf,_conts,shared) =
  M.fold_right shared ~init:[] ~f:(fun ~key:k ~data:v l -> (k,v)::l);;

let get_conts (_conf,conts,_shared) =
  M.fold_right conts ~init:[] ~f:(fun ~key:k ~data:v l -> (k,v)::l);;

let rec pp_maps maps =
  match maps with
      [] -> "=======================================\n"
    | (c,c')::maps' -> c ^ " -> " ^ c' ^ "\n" ^ pp_maps maps';; 

let pp_config config =
  let sems = get_sems config in
  let _maps = get_maps config in
  let _conts = get_conts config in
  pp_sems sems;;

let rec step env config =
  let sems = get_sems config in
  let () = print_string (pp_config config) in
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

(*

(* compute steps env config queue = final_config
 * steps are interactions or transitions taken to arrive at queue
 * env is global program
 * (List.rev queue) @ config is current configuration
 * config still to be scanned for possible transitions
 * queue has the processes resulting from transitions
 *)
(* wp = (work, potential) is unused and just blindly propagated
 * until we integrate this Fri Apr  6 08:45:49 2018 -fp *)

fun interactsL msg = case msg of
    M.LabL _ => true | M.NowL => true | M.PayL _ => true | M.AssertL _ => true
  | _ => false

fun interactsR msg = case msg of
    M.LabR _ => true | M.CloseR => true | M.NowR => true | M.PayR _ => true | M.AssertR _ => true
  | _ => false

datatype category = 
    RecvL 
  | RecvR
  | Fwd
  | Internal
  | MsgR
  | MsgL

fun cat_of proc = case proc of
    A.Proc(_,_,P) =>
      (case P of
        A.Cut _ => Internal
      | A.Spawn _ => Internal
      | A.Id => Fwd
      | A.LabR _ => Internal
      | A.CaseL _ => RecvL
      | A.LabL _ => Internal
      | A.CaseR _ => RecvR
      | A.CloseR => Internal
      | A.WaitL _ => RecvL
      | A.Delay _ => Internal
      | A.WhenR _ => RecvR
      | A.NowL _ => Internal
      | A.WhenL _ => RecvL
      | A.NowR _ => Internal
      | A.Work _ => Internal
      | A.PayR _ => Internal
      | A.PayL _ => Internal
      | A.GetR _ => RecvR
      | A.GetL _ => RecvL
      | A.AssertR _ => Internal
      | A.AssertL _ => Internal
      | A.AssumeR _ => RecvR
      | A.AssumeL _ => RecvL
      | A.Imposs => Internal
      | A.ExpName _ => Internal
      | A.Marked _ => raise MarkedExpCategory)
  | A.Msg(_,_,M) =>
      (case M of
        M.LabR _ => MsgR
      | M.LabL _ => MsgL
      | M.CloseR => MsgR
      | M.NowR => MsgR
      | M.NowL => MsgL
      | M.PayR _ => MsgR
      | M.PayL _ => MsgL
      | M.AssertR _ => MsgR
      | M.AssertL _ => MsgL)

fun is_final (P::(config as Q::config')) =
    (case (cat_of P, cat_of Q) of
      (MsgR, MsgL) => raise ProgressError (* type error *)
    | (MsgR, Fwd) => raise ProgressError (* can take a step *)
    | (MsgR, RecvL) => raise ProgressError (* can take a step or type error *)
    | (Fwd, MsgL) => raise ProgressError (* can take a step *)
    | (RecvR, RecvL) => raise ProgressError (* type error *)
    | (Internal, _) => raise ProgressError (* can take a step *)
    | _ => is_final config)
  | is_final (P::nil) =
    (case cat_of P of
      Internal => raise ProgressError (* can take a step *)
    | _ => ())
  | is_final nil = raise ProgressError (* empty configuration *)
*)

let error = ErrorMsg.error_msg ErrorMsg.Runtime None;;

let create_config sem =
  let config = (M.empty (module Chan), M.empty (module Chan), M.empty (module Chan)) in
  let config = add_sem sem config in
  config;;

(* exec env C = C'
 * C is a process configuration
 * env is the elaborated environment
 * C' is final, poised configuration
 *)
let exec env f =
  let c = lfresh () in
  let pot = try_evaluate (get_pot env f) in
  let sem = Proc(c,0,(0,pot),A.ExpName(c,f,[])) in
  try step env (create_config sem)
  with exn ->
    match exn with
        InsufficientPotential -> error "insufficient potential during execution"
                                 ; raise RuntimeError
      | UnconsumedPotential -> error "unconsumed potential during execution"
                               ; raise RuntimeError
      | PotentialMismatch -> error "potential mismatch during execution"
                             ; raise RuntimeError
      | MissingBranch -> error "missing branch during execution"
                         ; raise RuntimeError
      | ProgressError -> error "final configuration inconsistent"
                         ; raise RuntimeError
      | MarkedExpCategory -> error "marked expression found at runtime"
                             ; raise RuntimeError
      | ChannelMismatch -> error "channel name mismatch found at runtime"
                           ; raise RuntimeError
      | UndefinedProcess -> error "undefined process found at runtime"
                            ; raise RuntimeError
      | StarPotential -> error "potential * found at runtime"
                         ; raise RuntimeError
      | e -> raise e;;