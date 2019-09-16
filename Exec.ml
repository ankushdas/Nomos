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

exception RuntimeError (* should never happen at runtime *)

type sem =
    Proc of A.chan * int * (int * int) * A.expression   (* Proc(chan, time, (work, pot), P) *)
  | Msg of A.chan * int * (int * int) * A.msg           (* Msg(chan, time, (work, pot), M) *)

let pp_sem sem = match sem with
    Proc(c,t,(w,pot),p) ->
      "proc(" ^ c ^ ", " ^ string_of_int t ^ "(" ^ string_of_int w ^
      ", " ^ string_of_int pot ^ "), " ^ PP.pp_exp_prefix p ^ ")"
  | Msg(c,t,(w,pot),m) ->
      "msg(" ^ c ^ ", " ^ string_of_int t ^ "(" ^ string_of_int w ^
      ", " ^ string_of_int pot ^ "), " ^ PP.pp_msg m ^ ")";;

(* map from offered channel to semantic object *)
type map_string_sem = sem C.String.Map.t;;

(* map from channel to its continuation *)
type map_string_string = string C.String.Map.t;;

(* configuration type *)
type configuration = map_string_sem * map_string_string;;

let chan_num = ref 0;;

let lfresh () =
  let n = !chan_num in
  let () = chan_num := n+1 in
  "c" ^ (string_of_int n);;

let max(t,t') =
  if t > t' then t else t';;

let rec find_branch l bs =
  match bs with
      {A.lab_exp = (k,p); A.exp_extent = _ext}::bs' ->
        if k = l then p
        else find_branch l bs'
    | [] -> raise MissingBranch;;

let find_sem c (conf,_conts) =
  match M.find conf c with
      None -> raise ExecImpossible
    | Some v -> v;;

type pol = Pos | Neg;;

let find_msg c ((_conf,conts) as config) dual =
  match dual with
      Neg ->
        begin
          match M.find conts c with
              None -> None
            | Some c' -> Some (find_sem c' config)
        end
    | Pos -> Some (find_sem c config);;

let remove_sem c (conf,conts) =
  (M.remove conf c, conts);;

let add_sem sem (conf,conts) =
  match sem with
      Proc(c,_t,_wp,_p) ->
        (M.add_exn conf ~key:c ~data:sem, conts)
    | Msg(c,_t,_wp,_p) ->
        (M.add_exn conf ~key:c ~data:sem, conts);;

let add_cont (c,c') (conf,conts) =
  (conf, M.add_exn conts ~key:c ~data:c');;

let remove_cont c (conf,conts) =
  (conf, M.remove conts c);;

let get_pot env f =
  match A.lookup_expdec env f with
      None -> raise UndefinedProcess
    | Some(_ctx,pot,_zc) -> pot;;

let stepped = ref false;;

let spawn env ch config =
  let s = find_sem ch config in
  let config = remove_sem ch config in
  match s with
      Proc(d,t,(w,pot),A.Spawn(x,f,xs,q)) ->
        let c' = lfresh () in
        let pot' = R.evaluate (get_pot env f) in
        if pot < pot'
        then raise InsufficientPotential
        else
          let proc1 = Proc(c',t+1,(0,pot'),A.ExpName(c',f,xs)) in
          let proc2 = Proc(d,t+1,(w,pot-pot'),A.subst c' x q) in
          let config = add_sem proc1 config in
          let config = add_sem proc2 config in
          let () = stepped := true in
          config
    | _s -> raise ExecImpossible;;

let rec fst l = match l with
    (c,_t)::l' -> c::(fst l')
  | [] -> [];;

let expd_def env x f xs =
  match A.lookup_expdef env f with
      None -> raise UndefinedProcess
    | Some(exp) ->
        match A.lookup_expdec env f with
            None -> raise ExecImpossible
          | Some (ctx,_pot,(z,_c)) ->
              let exp = A.subst x z exp in
              let exp = A.subst_ctx xs (fst ctx.ordered) exp in
              exp;;

let expand env ch config =
  let s = find_sem ch config in
  let config = remove_sem ch config in
  match s with
      Proc(c,t,(w,pot),A.ExpName(x,f,xs)) ->
        let p = expd_def env x f xs in
        let pot' = R.evaluate (get_pot env f) in
        if pot <> pot'
        then raise PotentialMismatch
        else
          let proc = Proc(c,t,(w,pot),p) in
          let config = add_sem proc config in
          let () = stepped := true in
          config
    | _s -> raise ExecImpossible;;

let ichoice_S ch config =
  let s = find_sem ch config in
  let config = remove_sem ch config in
  match s with
      Proc(c1,t,wp,A.Lab(c2,l,p)) ->
        if c1 <> c2
        then raise ChannelMismatch
        else
          let c' = lfresh () in
          let msg = Msg(c1,t+1,(0,0),A.MLabI(c1,l,c')) in
          let proc = Proc(c',t+1,wp,A.subst c' c1 p) in
          let config = add_sem msg config in
          let config = add_sem proc config in
          let config = add_cont (c1,c') config in
          let () = stepped := true in
          config
    | _s -> raise ExecImpossible;;

let ichoice_R ch config =
  let s = find_sem ch config in
  match s with
      Proc(d,t,(w,pot),A.Case(c,bs)) ->
        if d = c
        then raise ChannelMismatch
        else
          let msg = find_msg c config Pos in
          begin
            match msg with
                Some(Msg(ceq, t', (w',pot'), A.MLabE(_ceq,l,c'))) ->
                  if ceq <> c
                  then raise ChannelMismatch
                  else
                    let q = find_branch l bs in
                    let proc = Proc(d, max(t,t')+1, (w+w',pot+pot'), A.subst c' c q) in
                    let config = remove_sem ch config in
                    let config = remove_sem c config in
                    let config = add_sem proc config in
                    let config = remove_cont c config in
                    let () = stepped := true in
                    config
              | _m -> config
          end
    | _s -> raise ExecImpossible;;

let echoice_S ch config =
  let s = find_sem ch config in
  let config = remove_sem ch config in
  match s with
      Proc(d,t,wp,A.Lab(c,l,p)) ->
        if d = c
        then raise ChannelMismatch
        else
          let c' = lfresh () in
          let msg = Msg(c',t+1,(0,0),A.MLabE(c,l,c')) in
          let proc = Proc(d,t+1,wp,A.subst c' c p) in
          let config = add_sem msg config in
          let config = add_sem proc config in
          let config = add_cont (c,c') config in
          let () = stepped := true in
          config
    | _s -> raise ExecImpossible;;

let echoice_R ch config =
  let s = find_sem ch config in
  match s with
      Proc(c1,t,(w,pot),A.Case(c2,bs)) ->
        if c1 <> c2
        then raise ChannelMismatch
        else
          let msg = find_msg c2 config Neg in
          begin
            match msg with
                Some(Msg(c2', t', (w',pot'), A.MLabE(c2eq,l,_c2'))) ->
                  if c2eq <> c2
                  then raise ChannelMismatch
                  else
                    let q = find_branch l bs in
                    let proc = Proc(c2', max(t,t')+1, (w+w',pot+pot'), A.subst c2' c2 q) in
                    let config = remove_sem ch config in
                    let config = remove_sem c2' config in
                    let config = add_sem proc config in
                    let config = remove_cont c2 config in
                    let () = stepped := true in
                    config
              | _m -> config
          end
    | _s -> config;;

let tensor_S ch config =
  let s = find_sem ch config in
  let config = remove_sem ch config in
  match s with
      Proc(c1,t,wp,A.Send(c2,e,p)) ->
        if c1 <> c2
        then raise ChannelMismatch
        else
          let c' = lfresh () in
          let msg = Msg(c1,t+1,(0,0),A.MSendT(c1,e,c')) in
          let proc = Proc(c',t+1,wp,A.subst c' c1 p) in
          let config = add_sem msg config in
          let config = add_sem proc config in
          let config = add_cont (c1,c') config in
          let () = stepped := true in
          config
    | _s -> raise ExecImpossible;;

let tensor_R ch config =
  let s = find_sem ch config in
  match s with
      Proc(d,t,(w,pot),A.Recv(c,x,q)) ->
        if d = c
        then raise ChannelMismatch
        else
          let msg = find_msg c config Pos in
          begin
            match msg with
                Some(Msg(ceq, t', (w',pot'), A.MSendT(_ceq,e,c'))) ->
                  if ceq <> c
                  then raise ChannelMismatch
                  else
                    let q = A.subst e x q in
                    let proc = Proc(d, max(t,t')+1, (w+w',pot+pot'), A.subst c' c q) in
                    let config = remove_sem ch config in
                    let config = remove_sem c config in
                    let config = add_sem proc config in
                    let config = remove_cont c config in
                    let () = stepped := true in
                    config
              | _m -> config
          end
    | _s -> raise ExecImpossible;;
        
let lolli_S ch config =
  let s = find_sem ch config in
  let config = remove_sem ch config in
  match s with
      Proc(d,t,wp,A.Send(c,e,p)) ->
        if d = c
        then raise ChannelMismatch
        else
          let c' = lfresh () in
          let msg = Msg(c',t+1,(0,0),A.MSendL(c,e,c')) in
          let proc = Proc(d,t+1,wp,A.subst c' c p) in
          let config = add_sem msg config in
          let config = add_sem proc config in
          let config = add_cont (c,c') config in
          let () = stepped := true in
          config
    | _s -> raise ExecImpossible;;

let lolli_R ch config =
  let s = find_sem ch config in
  match s with
      Proc(c1,t,(w,pot),A.Recv(c2,x,q)) ->
        if c1 <> c2
        then raise ChannelMismatch
        else
          let msg = find_msg c2 config Neg in
          begin
            match msg with
                Some(Msg(c2', t', (w',pot'), A.MSendL(c2eq,e,_c2'))) ->
                  if c2eq <> c2
                  then raise ExecImpossible
                  else
                    let q = A.subst e x q in
                    let proc = Proc(c2', max(t,t')+1, (w+w',pot+pot'), A.subst c2' c2 q) in
                    let config = remove_sem ch config in
                    let config = remove_sem c2' config in
                    let config = add_sem proc config in
                    let config = remove_cont c2 config in
                    let () = stepped := true in
                    config
              | _m -> config
          end
    | _s -> raise ExecImpossible;;

let one_S ch config =
  let s = find_sem ch config in
  let config = remove_sem ch config in
  match s with
      Proc(c1,t,(w,pot),A.Close(c2)) ->
        if c1 <> c2
        then raise ChannelMismatch
        else if pot > 0
        then raise UnconsumedPotential
        else
          let msg = Msg(c1,t+1,(w,pot),A.MClose(c1)) in
          let config = add_sem msg config in
          let () = stepped := true in
          config
    | _s -> raise ExecImpossible;;

let one_R ch config =
  let s = find_sem ch config in
  let config = remove_sem ch config in
  match s with
      Proc(d,t,(w,pot),A.Wait(c,q)) ->
        if d = c
        then raise ChannelMismatch
        else
          let msg = find_msg c config Pos in
          begin
            match msg with
                Some(Msg(ceq, t', (w',pot'), A.MClose(_ceq))) ->
                  if ceq <> c
                  then raise ExecImpossible
                  else
                    let proc = Proc(d, max(t,t')+1, (w+w',pot+pot'), q) in
                    let config = remove_sem c config in
                    let config = add_sem proc config in
                    let () = stepped := true in
                    config
              | _m -> raise ExecImpossible
          end
    | _s -> raise ExecImpossible;;

let work ch config =
  let s = find_sem ch config in
  let config = remove_sem ch config in
  match s with
      Proc(c,t,(w,pot),A.Work(k,p)) ->
        let k = R.evaluate k in
        if pot < k
        then raise InsufficientPotential
        else
          let proc = Proc(c,t+1,(w+k,pot-k),p) in
          let config = add_sem proc config in
          let () = stepped := true in
          config
    | _s -> raise ExecImpossible;;

let paypot_S ch config =
  let s = find_sem ch config in
  let config = remove_sem ch config in
  match s with
      Proc(c1,t,(w,pot),A.Pay(c2,epot,p)) ->
        if c1 <> c2
        then raise ChannelMismatch
        else if pot < R.evaluate epot
        then raise InsufficientPotential
        else
          let c' = lfresh () in
          let vpot = R.evaluate epot in
          let msg = Msg(c1,t+1,(0,vpot),A.MPayP(c1,epot,c')) in
          let proc = Proc(c',t+1,(w,pot-vpot),A.subst c' c1 p) in
          let config = add_sem msg config in
          let config = add_sem proc config in
          let config = add_cont (c1,c') config in
          let () = stepped := true in
          config
    | _s -> raise ExecImpossible;;

let paypot_R ch config =
  let s = find_sem ch config in
  match s with
      Proc(d,t,(w,pot),A.Get(c,epot,q)) ->
        if d = c
        then raise ChannelMismatch
        else
          let msg = find_msg c config Pos in
          begin
            match msg with
                Some(Msg(ceq, t', (w',pot'), A.MPayP(_ceq,epot',c'))) ->
                  if ceq <> c
                  then raise ChannelMismatch
                  else if not (R.eq epot epot')
                  then raise PotentialMismatch
                  else
                    let proc = Proc(d, max(t,t')+1, (w+w',pot+pot'), A.subst c' c q) in
                    let config = remove_sem ch config in
                    let config = remove_sem c config in
                    let config = add_sem proc config in
                    let config = remove_cont c config in
                    let () = stepped := true in
                    config
              | _m -> config
          end
    | _s -> raise ExecImpossible;;
        
let getpot_S ch config =
  let s = find_sem ch config in
  let config = remove_sem ch config in
  match s with
      Proc(d,t,(w,pot),A.Pay(c,epot,p)) ->
        if d = c
        then raise ChannelMismatch
        else if pot < R.evaluate epot
        then raise InsufficientPotential
        else
          let c' = lfresh () in
          let vpot = R.evaluate epot in
          let msg = Msg(c',t+1,(0,vpot),A.MPayG(c,epot,c')) in
          let proc = Proc(d,t+1,(w,pot-vpot),A.subst c' c p) in
          let config = add_sem msg config in
          let config = add_sem proc config in
          let config = add_cont (c,c') config in
          let () = stepped := true in
          config
    | _s -> raise ExecImpossible;;

let getpot_R ch config =
  let s = find_sem ch config in
  match s with
      Proc(c1,t,(w,pot),A.Get(c2,epot,q)) ->
        if c1 <> c2
        then raise ChannelMismatch
        else
          let msg = find_msg c2 config Neg in
          begin
            match msg with
                Some(Msg(c2', t', (w',pot'), A.MPayG(c2eq,epot',_c2'))) ->
                  if c2eq <> c2
                  then raise ChannelMismatch
                  else if not (R.eq epot epot')
                  then raise PotentialMismatch
                  else
                    let proc = Proc(c2', max(t,t')+1, (w+w',pot+pot'), A.subst c2' c2 q) in
                    let config = remove_sem ch config in
                    let config = remove_sem c2' config in
                    let config = add_sem proc config in
                    let config = remove_cont c2 config in
                    let () = stepped := true in
                    config
              | _m -> config
          end
    | _s -> raise ExecImpossible;;

let match_and_one_step env sem config =
  match sem with
      Proc(c,_t,_wp,p) ->
        begin
          match p with
              A.Fwd(c',d') ->
                (* TODO: how to handle forward *)
                config
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
            
            | A.Marked _ ->
                raise MarkedExpCategory
            
            | _ -> config
        end
    | Msg _ -> config;;

let get_vals (conf,_conts) =
  M.fold_right conf ~init:[] ~f:(fun ~key:_k ~data:v l -> v::l);;
          
let rec step env config =
  let sems = get_vals config in
  let config = iterate_and_one_step env sems config in
  config

and iterate_and_one_step env sems config =
  stepped := false ;
  match sems with
      [] -> if !stepped then step env config else config
    | sem::sems' ->
        let config = match_and_one_step env sem config in
        iterate_and_one_step env sems' config;; 

let rec print_list vs = match vs with
    [] -> ()
  | v::vs' -> print_string (v ^ "\n") ; print_list vs';;

let () =
  let m = M.empty (module C.String) in
  let m = M.add_exn m ~key:"a" ~data:"c" in
  let m = M.add_exn m ~key:"c" ~data:"e" in
  let m = M.add_exn m ~key:"b" ~data:"d" in
  let vs = get_vals (m,()) in
  print_list vs;;
(*
  | compute steps env (A.Msg(t',(w',p'),M)::A.Proc(t,(w,p),A.Id)::config) queue =
    if interactsR M then
      if not(p = 0) then raise UnconsumedPotential
      else if not(t' >= t) then raise TimeMismatch
      else compute (steps+1) env config (A.Msg(t',(w+w',p+p'),M)::queue)
    else compute steps env (A.Proc(t,(w,p),A.Id)::config) (A.Msg(t',(w',p'),M)::queue)
  | compute steps env (A.Proc(t,(w,p),A.Id)::A.Msg(t',(w',p'),M)::config) queue =
    if interactsL M then
      if not(p = 0) then raise UnconsumedPotential
      else if not(t' >= t) then raise TimeMismatch
      else compute (steps+1) env config (A.Msg(t',(w+w',p+p'),M)::queue)
    else compute steps env (A.Msg(t',(w',p'),M)::config) (A.Proc(t,(w,p),A.Id)::queue)
  | compute steps env nil queue =
    let val config = List.rev queue
    in
      if steps = 0                    (* no steps taken; done *)
      then
        let val () = is_final config
        in
          config
        end
      else compute 0 env config nil
    end
*)
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
  let config = (M.empty (module C.String), M.empty (module C.String)) in
  let config = add_sem sem config in
  config;;

(* exec env C = C'
 * C is a process configuration
 * env is the elaborated environment
 * C' is final, poised configuration
 *)
let exec env sem =
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
      | e -> raise e;;