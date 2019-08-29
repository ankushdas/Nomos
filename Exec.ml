module R = Arith
module A = Ast
module PP = Pprint
module M = Map.Make(String)

exception InsufficientPotential (* not enough potential to work or pay *)

exception UnconsumedPotential (* too much potential during termination *)

exception PotentialMismatch (* mismatch while spawning or exchanging *)

exception MissingBranch (* missing branch during internal/external choice *)

exception MarkedExpCategory (* marked expression found at runtime *)

exception ProgressError (* final configuration inconsistent *)

exception ChannelMismatch (* channel name mismatch in sem *)

exception ExecImpossible (* should never happen at runtime *)

type sem =
    Proc of A.chan * int * (int * int) * A.expression   (* Proc(chan, time, (work, pot), P) *)
  | Msg of A.chan * int * (int * int) * A.msg           (* Msg(chan, time, (work, pot), M) *)

let pp_sem semobj = match semobj with
    Proc(c,t,(w,pot),p) ->
      "proc(" ^ c ^ ", " ^ string_of_int t ^ "(" ^ string_of_int w ^
      ", " ^ string_of_int pot ^ "), " ^ PP.pp_exp_prefix p ^ ")"
  | Msg(c,t,(w,pot),m) ->
      "msg(" ^ c ^ ", " ^ string_of_int t ^ "(" ^ string_of_int w ^
      ", " ^ string_of_int pot ^ "), " ^ PP.pp_msg m ^ ")";;

type configuration = sem M.t;;

let fwd_p d s l = match s with
    Proc(c1,t,(w,pot),A.Fwd(c2,d)) ->
      let s' = M.find d l in
      let l' = M.remove d l in

      ()
  | _s -> raise ExecImpossible

(*
let rec one_step steps env config =
  match config with
      Leaf -> Leaf
    | Node(s, l) ->
        match s with
          (* fwd+ *)
            A.Proc(c1,t,(w,pot),A.Fwd(c2,d)) ->
              if c1 <> c2
              then raise ChannelMismatch
              else if pot > 0
              then raise UnconsumedPotential
              else fwd_p d s l
*)
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