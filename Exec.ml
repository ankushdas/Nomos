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

exception ExecImpossible (* should never happen at runtime *)

exception FinalConfig

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
    | [] -> raise ExecImpossible;;

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
              None -> raise ExecImpossible
            | Some c' -> find_sem c' config
        end
    | Pos -> find_sem c config;;

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
      None -> raise ExecImpossible
    | Some(_ctx,pot,_zc) -> pot;;

let spawn env ch config =
  let s = find_sem ch config in
  let config = remove_sem ch config in
  match s with
      Proc(d,t,(w,pot),A.Spawn(x,f,xs,q)) ->
        let c' = lfresh () in
        let pot' = R.evaluate (get_pot env f) in
        let proc1 = Proc(c',t+1,(0,pot'),A.ExpName(c',f,xs)) in
        let proc2 = Proc(d,t+1,(w,pot-pot'),A.subst c' x q) in
        let config = add_sem proc1 config in
        let config = add_sem proc2 config in
        config
    | _s -> raise ExecImpossible;;

let rec fst l = match l with
    (c,_t)::l' -> c::(fst l')
  | [] -> [];;

let expd_def env x f xs =
  match A.lookup_expdef env f with
      None -> raise ExecImpossible
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
      Proc(c,t,wp,A.ExpName(x,f,xs)) ->
        let p = expd_def env x f xs in
        let proc = Proc(c,t,wp,p) in
        let config = add_sem proc config in
        config
    | _s -> raise ExecImpossible;;

let ichoice_S ch config =
  let s = find_sem ch config in
  let config = remove_sem ch config in
  match s with
      Proc(c1,t,wp,A.Lab(c2,l,p)) ->
        if c1 <> c2
        then raise ExecImpossible
        else
          let c' = lfresh () in
          let msg = Msg(c1,t+1,(0,0),A.MLabI(c1,l,c')) in
          let proc = Proc(c',t+1,wp,A.subst c' c1 p) in
          let config = add_sem msg config in
          let config = add_sem proc config in
          let config = add_cont (c1,c') config in
          config
    | _s -> raise ExecImpossible;;

let ichoice_R ch config =
  let s = find_sem ch config in
  let config = remove_sem ch config in
  match s with
      Proc(d,t,(w,pot),A.Case(c,bs)) ->
        if d = c
        then raise ExecImpossible
        else
          let msg = find_msg c config Pos in
          begin
            match msg with
                Msg(ceq, t', (w',pot'), A.MLabE(_ceq,l,c')) ->
                  if ceq <> c
                  then raise ExecImpossible
                  else
                    let q = find_branch l bs in
                    let proc = Proc(d, max(t,t')+1, (w+w',pot+pot'), A.subst c' c q) in
                    let config = remove_sem c config in
                    let config = add_sem proc config in
                    let config = remove_cont c config in
                    config
              | _m -> raise ExecImpossible
          end
    | _s -> raise ExecImpossible;;

let echoice_S ch config =
  let s = find_sem ch config in
  let config = remove_sem ch config in
  match s with
      Proc(d,t,wp,A.Lab(c,l,p)) ->
        if d = c
        then raise ExecImpossible
        else
          let c' = lfresh () in
          let msg = Msg(c',t+1,(0,0),A.MLabE(c,l,c')) in
          let proc = Proc(d,t+1,wp,A.subst c' c p) in
          let config = add_sem msg config in
          let config = add_sem proc config in
          let config = add_cont (c,c') config in
          config
    | _s -> raise ExecImpossible;;

let echoice_R ch config =
  let s = find_sem ch config in
  let config = remove_sem ch config in
  match s with
      Proc(c1,t,(w,pot),A.Case(c2,bs)) ->
        if c1 <> c2
        then raise ExecImpossible
        else
          let msg = find_msg c2 config Neg in
          begin
            match msg with
                Msg(c2', t', (w',pot'), A.MLabE(c2eq,l,_c2')) ->
                  if c2eq <> c2
                  then raise ExecImpossible
                  else
                    let q = find_branch l bs in
                    let proc = Proc(c2', max(t,t')+1, (w+w',pot+pot'), A.subst c2' c2 q) in
                    let config = remove_sem c2' config in
                    let config = add_sem proc config in
                    let config = remove_cont c2 config in
                    config
              | _m -> raise ExecImpossible
          end
    | _s -> raise ExecImpossible;;

let tensor_S ch config =
  let s = find_sem ch config in
  let config = remove_sem ch config in
  match s with
      Proc(c1,t,wp,A.Send(c2,e,p)) ->
        if c1 <> c2
        then raise ExecImpossible
        else
          let c' = lfresh () in
          let msg = Msg(c1,t+1,(0,0),A.MSendT(c1,e,c')) in
          let proc = Proc(c',t+1,wp,A.subst c' c1 p) in
          let config = add_sem msg config in
          let config = add_sem proc config in
          let config = add_cont (c1,c') config in
          config
    | _s -> raise ExecImpossible;;

let tensor_R ch config =
  let s = find_sem ch config in
  let config = remove_sem ch config in
  match s with
      Proc(d,t,(w,pot),A.Recv(c,x,q)) ->
        if d = c
        then raise ExecImpossible
        else
          let msg = find_msg c config Pos in
          begin
            match msg with
                Msg(ceq, t', (w',pot'), A.MSendT(_ceq,e,c')) ->
                  if ceq <> c
                  then raise ExecImpossible
                  else
                    let q = A.subst e x q in
                    let proc = Proc(d, max(t,t')+1, (w+w',pot+pot'), A.subst c' c q) in
                    let config = remove_sem c config in
                    let config = add_sem proc config in
                    let config = remove_cont c config in
                    config
              | _m -> raise ExecImpossible
          end
    | _s -> raise ExecImpossible;;
        
let lolli_S ch config =
  let s = find_sem ch config in
  let config = remove_sem ch config in
  match s with
      Proc(d,t,wp,A.Send(c,e,p)) ->
        if d = c
        then raise ExecImpossible
        else
          let c' = lfresh () in
          let msg = Msg(c',t+1,(0,0),A.MSendL(c,e,c')) in
          let proc = Proc(d,t+1,wp,A.subst c' c p) in
          let config = add_sem msg config in
          let config = add_sem proc config in
          let config = add_cont (c,c') config in
          config
    | _s -> raise ExecImpossible;;

let lolli_R ch config =
  let s = find_sem ch config in
  let config = remove_sem ch config in
  match s with
      Proc(c1,t,(w,pot),A.Recv(c2,x,q)) ->
        if c1 <> c2
        then raise ExecImpossible
        else
          let msg = find_msg c2 config Neg in
          begin
            match msg with
                Msg(c2', t', (w',pot'), A.MSendL(c2eq,e,_c2')) ->
                  if c2eq <> c2
                  then raise ExecImpossible
                  else
                    let q = A.subst e x q in
                    let proc = Proc(c2', max(t,t')+1, (w+w',pot+pot'), A.subst c2' c2 q) in
                    let config = remove_sem c2' config in
                    let config = add_sem proc config in
                    let config = remove_cont c2 config in
                    config
              | _m -> raise ExecImpossible
          end
    | _s -> raise ExecImpossible;;
    
let match_and_step env stepped vs config =
  match vs with
      [] -> if stepped then config else raise FinalConfig
    | sem::sems ->
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
                  | _ -> config
              end
          | Msg(c,_t,_wp,m) -> config;;

let get_vals (conf,_conts) =
  M.fold_right conf ~init:[] ~f:(fun ~key:_k ~data:v l -> v::l);;
          
let one_step env stepped config =
  let vs = get_vals config in
  match_and_step env stepped vs config;;

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
fun compute steps env (A.Proc(t,(w,p),A.Cut(P,pote,A,Q))::config) queue =
    let val pot = R.evaluate pote
    in
      if not(p >= pot) then raise InsufficientPotential
      else compute (steps+1) env config (A.Proc(t,(w,p-pot),Q)::A.Proc(t,(0,pot),P)::queue)
    end
  | compute steps env (A.Proc(t,(w,p),A.Spawn(P,Q))::config) queue = (* do not count spawn -> cut as step *)
    compute steps env (A.Proc(t,(w,p),TypeCheck.syn_cut env (P,Q) NONE)::config) queue
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
  (*
  | compute steps env (A.Proc(t,(w,p),A.Id)::config) queue =
    if not(p = 0) then raise UnconsumedPotential
    else compute (steps+1) env config queue
  *)
  | compute steps env (A.Proc(t,wp,A.LabR(k,P))::config) queue = (* no-cost send *)
    compute (steps+1) env config (A.Msg(t,(0,0),M.LabR(k))::A.Proc(t,wp,P)::queue)
  | compute steps env (A.Msg(t,wp,M.LabR(k))::A.Proc(t',wp',A.CaseL(branches))::config) queue =
    if t <> t' then raise TimeMismatch
    else compute (steps+1) env config (A.Proc(t,plus wp wp',branch_check (A.lookup_branch branches k))::queue)
  | compute steps env (A.Proc(t,wp,A.CaseR(branches))::A.Msg(t',wp',M.LabL(k))::config) queue =
    if t <> t' then raise TimeMismatch
    else compute (steps+1) env config (A.Proc(t,plus wp wp',branch_check (A.lookup_branch branches k))::queue)
  | compute steps env (A.Proc(t,wp,A.LabL(k,Q))::config) queue = (* no-cost send *)
    compute (steps+1) env config (A.Proc(t,wp,Q)::A.Msg(t,(0,0),M.LabL(k))::queue)
  | compute steps env (A.Proc(t,(w,p),A.CloseR)::config) queue =
    if not(p = 0) then raise UnconsumedPotential
    else compute (steps+1) env config (A.Msg(t,(w,p),M.CloseR)::queue)
  | compute steps env (A.Msg(t,wp,M.CloseR)::A.Proc(t',wp',A.WaitL(Q))::config) queue =
    if t <> t' then raise TimeMismatch
    else compute (steps+1) env config (A.Proc(t,plus wp wp',Q)::queue)
  | compute steps env (A.Proc(t,wp,A.Delay(te',P))::config) queue =
    let val t' = R.evaluate te'
    in
    compute (steps+1) env config (A.Proc(t+t',wp,P)::queue)
    end
  | compute steps env (A.Proc(s,wp,A.NowL(Q))::config) queue =
    compute (steps+1) env config (A.Proc(s,wp,Q)::A.Msg(s,(0,0),M.NowL)::queue)
  | compute steps env (A.Proc(s,wp',A.WhenR(P))::A.Msg(t,wp,M.NowL)::config) queue =
    if not(s <= t) then raise TimeMismatch
    else compute (steps+1) env config (A.Proc(t,plus wp wp',P)::queue)
  | compute steps env (A.Proc(t,wp,A.NowR(P))::config) queue =
    compute (steps+1) env config (A.Msg(t,(0,0),M.NowR)::A.Proc(t,wp,P)::queue)
  | compute steps env (A.Msg(t,wp,M.NowR)::A.Proc(s,wp',A.WhenL(Q))::config) queue =
    if not(s <= t) then raise TimeMismatch
    else compute (steps+1) env config (A.Proc(t,plus wp wp',Q)::queue)
  | compute steps env (A.Proc(t,(w, p), A.Work(pe',P))::config) queue =
    let val p' = R.evaluate pe'
    in
      if not(p >= p') then raise InsufficientPotential
      else compute (steps+1) env config (A.Proc(t,(w+p',p-p'),P)::queue)
    end
  | compute steps env (A.Proc(t,(w,p),A.PayR(pe',P))::config) queue =
    let val p' = R.evaluate pe'
    in
      if not(p >= p') then raise InsufficientPotential
      else compute (steps+1) env config (A.Msg(t,(0,p'),M.PayR(pe'))::A.Proc(t,(w,p-p'),P)::queue)
    end
  | compute steps env (A.Msg(t,wp,M.PayR(p1e))::A.Proc(t',wp',A.GetL(p2e,Q))::config) queue =
    let val p1 = R.evaluate p1e
        val p2 = R.evaluate p2e
    in
      if t <> t' then raise TimeMismatch
      else if p1 <> p2 then raise PotentialMismatch
      else compute (steps+1) env config (A.Proc(t',plus wp wp',Q)::queue)
    end
  | compute steps env (A.Proc(t,(w,p),A.PayL(pe',P))::config) queue =
    let val p' = R.evaluate pe'
    in
      if not(p >= p') then raise InsufficientPotential
      else compute (steps+1) env config (A.Proc(t,(w,p-p'),P)::A.Msg(t,(0,p'),M.PayL(pe'))::queue)
    end
  | compute steps env (A.Proc(t,wp,A.GetR(p1e,P))::A.Msg(t',wp',M.PayL(p2e))::config) queue =
    let val p1 = R.evaluate p1e
        val p2 = R.evaluate p2e
    in
      if t <> t' then raise TimeMismatch
      else if p1 <> p2 then raise PotentialMismatch
      else compute (steps+1) env config (A.Proc(t,plus wp wp',P)::queue)
    end
  | compute steps env (A.Proc(t,wp,A.AssertR(phi,Q))::config) queue =
    if not(R.evaluate_prop phi) then raise AssertionFailure
    else compute (steps+1) env config (A.Msg(t,(0,0),M.AssertR(phi))::A.Proc(t,wp,Q)::queue)
  | compute steps env (A.Msg(t,(0,0),M.AssertR(phi))::A.Proc(t',wp',A.AssumeL(phi',Q))::config) queue =
    if t <> t' then raise TimeMismatch
    else if not(R.evaluate_prop phi') then raise AssumptionFailure
    else compute (steps+1) env config (A.Proc(t',wp',Q)::queue)
  | compute steps env (A.Proc(t,wp,A.AssertL(phi,P))::config) queue =
    if not(R.evaluate_prop phi) then raise AssertionFailure
    else compute (steps+1) env config (A.Proc(t,wp,P)::A.Msg(t,(0,0),M.AssertL(phi))::queue)
  | compute steps env (A.Proc(t,wp,A.AssumeR(phi,P))::A.Msg(t',(0,0),M.AssertL(phi'))::config) queue =
    if t <> t' then raise TimeMismatch
    else if not(R.evaluate_prop phi) then raise AssumptionFailure
    else compute (steps+1) env config (A.Proc(t,wp,P)::queue)

  | compute steps env (A.Proc(t,wp,A.Imposs)::config) queue =
    raise ImpossibleReached

  | compute steps env (A.Proc(t,(w,p),A.ExpName(f,es))::config) queue =
    (case (A.lookup_expdef env f, A.lookup_expdec env f)
      of (SOME(vs,P), SOME(vs',con,(_,pot,_))) =>
              let val sg' = zip_check f vs' es
                  val pots = R.apply sg' pot
                  val potv = R.evaluate pots
                  val con' = R.apply_prop sg' con
                  val sg = zip_check f vs es
              in
                  if not(p=potv) then raise PotentialMismatch
                  else if not(R.evaluate_prop con') then raise ConstraintFailure
                  else compute (steps+1) env config (A.Proc(t,(w,p),A.apply_exp sg P)::queue)
              end
       | (_,_) => ( ErrorMsg.error NONE ("process " ^ f ^ " undefined or undeclared")
                  ; raise ErrorMsg.Error ))
  | compute steps env (p::config) queue = compute steps env config (p::queue)
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
fun plus (w,p) (w',p') = (w+w',p+p')

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

fun zip_check f vs es =
    (R.zip vs es)
    handle ListPair.UnequalLengths => raise IncorrectIndicesLength

fun branch_check (NONE) = raise MissingBranch
  | branch_check (SOME(P)) = P

(* exec env C = C'
 * C is a process configuration
 * env is the elaborated environment
 * C' is final, poised configuration
 *)
fun exec env C =
    (compute 0 env C nil)
    handle TimeMismatch => ( ErrorMsg.error NONE "time mismatch during execution"
                           ; raise SoftError )
        |  InsufficientPotential => ( ErrorMsg.error NONE "insufficient potential during execution"
                                    ; raise SoftError )
        |  UnconsumedPotential => ( ErrorMsg.error NONE "unconsumed potential during execution"
                                  ; raise SoftError )
        |  PotentialMismatch => ( ErrorMsg.error NONE "potential mismatch during execution"
                                    ; raise SoftError )
        | AssertionFailure => ( ErrorMsg.error NONE "assertion failed during execution"
                              ; raise SoftError )
        | AssumptionFailure => ( ErrorMsg.error NONE "assumption failure during execution"
                               ; raise SoftError )
        | ImpossibleReached => ( ErrorMsg.error NONE "impossible branch reached during execution"
                               ; raise SoftError )
        | ConstraintFailure => ( ErrorMsg.error NONE "constraint failed during execution"
                               ; raise SoftError )
        | R.NotClosed => ( ErrorMsg.error NONE "open expression in potential during execution"
                                    ; raise HardError )
        | IncorrectIndicesLength => ( ErrorMsg.error NONE "index length mismatch during execution"
                                    ; raise HardError )
        | MissingBranch => ( ErrorMsg.error NONE "missing branch during execution"
                           ; raise HardError )
        | ProgressError => ( ErrorMsg.error NONE "final configuration inconsistent"
                           ; raise HardError )
        | MarkedExpCategory => ( ErrorMsg.error NONE "marked expression found at runtime"
                           ; raise HardError )

*)