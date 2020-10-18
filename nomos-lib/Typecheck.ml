(* Type Checking *)
(* Use the syntax-directed rules to check the types and
* raises ErrorMsg.Error if an error is discovered
*)

module R = Arith
module A = Ast
module PP = Pprint
module E = TpError
module I = Infer
module F = NomosFlags

let error = ErrorMsg.error ErrorMsg.Type;;

(*********************)
(* Validity of types *)
(*********************)


(*
  Equi-Synchronizing Session Types
  Purely linear types are always equi-synchronizing
*)

let rec esync env seen tp c ext is_shared =
  if !F.verbosity >= 3
  then print_string ("checking esync: \n" ^ PP.pp_tp env tp ^ "\n" ^ PP.pp_tp env c ^ "\n") ;
  match tp with
      A.Plus(choice) -> esync_choices env seen choice c ext is_shared
    | A.With(choice) -> esync_choices env seen choice c ext is_shared
    | A.PPlus(pchoices) -> esync_pchoices env seen pchoices c ext is_shared
    | A.PWith(pchoices) -> esync_pchoices env seen pchoices c ext is_shared
    | A.Tensor(_a,b,_m) -> esync env seen b c ext is_shared
    | A.Lolli(_a,b,_m) -> esync env seen b c ext is_shared
    | A.One -> if is_shared then error ext ("type not equi-synchronizing") else ()
    | A.PayPot(_pot,a) -> esync env seen a c ext is_shared
    | A.GetPot(_pot,a) -> esync env seen a c ext is_shared
    | A.TpName(v) ->
        begin
          try
            if List.exists (fun x -> x = v) seen
            then ()
            else esync env (v::seen) (A.expd_tp env v) c ext is_shared
          with
            A.UndeclaredTp -> error ext ("type " ^ v ^ " undeclared")
        end
    | A.Up(a) -> esync env seen a c ext true
    | A.Down(a) -> esync env seen a c ext true
    | A.FArrow(_t,a) -> esync env seen a c ext is_shared
    | A.FProduct(_t,a) -> esync env seen a c ext is_shared

and esync_pchoices env seen pcs c ext is_shared = match pcs with
    (_l,_pot,a)::as' -> esync env seen a c ext is_shared ; esync_pchoices env seen as' c ext is_shared
  | [] -> ()

and esync_choices env seen cs c ext is_shared = match cs with
    (_l,a)::as' -> esync env seen a c ext is_shared ; esync_choices env seen as' c ext is_shared
  | [] -> ();;

let esync_tp env tp ext = esync env [] tp tp ext false;;

(***********************)
(* Properties of types *)
(***********************)

let contractive tp = match tp with
    A.TpName(_a) -> false
  | A.Plus _ | A.With _
  | A.PPlus _ | A.PWith _ 
  | A.Tensor _ | A.Lolli _
  | A.One
  | A.PayPot _ | A.GetPot _
  | A.Up _ | A.Down _
  | A.FArrow _ | A.FProduct _ -> true;;

(*****************)
(* Type equality *)
(*****************)


(* Type equality, equirecursively defined *)

(* Structural equality *)

let zero = A.Arith (R.Float 0.);;

let eq pot1 pot2 = match pot1, pot2 with
    A.Star, A.Star
  | A.Star, A.Arith _
  | A.Arith _, A.Star -> true
  | A.Arith p1, A.Arith p2 -> I.eq p1 p2;;

let ge pot1 pot2 = match pot1, pot2 with
    A.Star, A.Star
  | A.Star, A.Arith _
  | A.Arith _, A.Star -> true
  | A.Arith p1, A.Arith p2 -> I.ge p1 p2;;

let minus pot1 pot2 = match pot1, pot2 with
    A.Star, A.Star
  | A.Star, A.Arith _
  | A.Arith _, A.Star -> A.Star
  | A.Arith p1, A.Arith p2 -> A.Arith (R.minus p1 p2);;

let plus pot1 pot2 = match pot1, pot2 with
    A.Star, A.Star
  | A.Star, A.Arith _
  | A.Arith _, A.Star -> A.Star
  | A.Arith p1, A.Arith p2 -> A.Arith (R.plus p1 p2);;

let pp_uneq pot1 pot2 = match pot1, pot2 with
    A.Star, A.Star -> "* != *"
  | A.Arith p1, A.Star -> R.pp_arith p1 ^ " != *"
  | A.Star, A.Arith p2 -> "* != " ^ R.pp_arith p2
  | A.Arith p1, A.Arith p2 -> R.pp_uneq p1 p2;;

let pp_lt pot1 pot2 = match pot1, pot2 with
  A.Star, A.Star -> "* < *"
| A.Arith p1, A.Star -> R.pp_arith p1 ^ " < *"
| A.Star, A.Arith p2 -> "* < " ^ R.pp_arith p2
| A.Arith p1, A.Arith p2 -> R.pp_lt p1 p2;;

let mode_L (_s,_c,m) = match m with
    A.Linear
  | A.Unknown -> true
  | A.MVar v -> I.m_eq_const v A.Linear
  | _ -> false;;

let mode_S (_s,_c,m) = match m with
    A.Shared
  | A.Unknown -> true
  | A.MVar v -> I.m_eq_const v A.Shared
  | _ -> false;;

let mode_P (_s,_c,m) = match m with
    A.Pure
  | A.Unknown -> true
  | A.MVar v -> I.m_eq_const v A.Pure
  | _ -> false;;

let mode_T (_s,_c,m) = match m with
    A.Transaction
  | A.Unknown -> true
  | A.MVar v -> I.m_eq_const v A.Transaction
  | _ -> false;;

let mode_lin (_s,_c,m) = match m with
    A.Pure
  | A.Linear
  | A.Transaction
  | A.Unknown -> true
  | A.MVar v -> I.m_lin v
  | _ -> false;;

let eqmode m1 m2 = match m1, m2 with
    A.Pure, A.Pure
  | A.Linear, A.Linear
  | A.Transaction, A.Transaction
  | A.Shared, A.Shared
  | A.Unknown, _
  | _, A.Unknown -> true
  | A.MVar v1, A.MVar v2 -> I.m_eq v1 v2
  | A.MVar v, _ -> I.m_eq_const v m2
  | _, A.MVar v -> I.m_eq_const v m1
  | _, _ -> false;;

let mode_spawn m1 m2 = match m1, m2 with
    A.Pure, A.Pure
  | A.Pure, A.Shared
  | A.Pure, A.Linear
  | A.Pure, A.Transaction
  | A.Shared, A.Shared
  | A.Shared, A.Linear
  | A.Shared, A.Transaction
  | A.Transaction, A.Transaction -> true
  | _, _ -> false;;

let mode_recv m1 m2 = match m1, m2 with
    A.Unknown, _ -> true
  | A.Pure, A.Pure -> true
  | A.MVar v, A.Pure -> I.m_eq_const v A.Pure
  | _, A.Pure -> false
  | A.Pure, A.Shared
  | A.Shared, A.Shared -> true
  | A.MVar v, A.Shared -> I.m_eq_pair v A.Pure A.Shared
  | _, A.Shared -> false
  | _, A.Linear -> true
  | _, A.Transaction -> true
  | _, _ -> false;;

let rec eq_ftp tp tp' = match tp, tp' with
    A.Integer, A.Integer -> true
  | A.Boolean, A.Boolean -> true
  | A.String, A.String -> true
  | A.Address, A.Address -> true
  | A.ListTP(t,pot), A.ListTP(t',pot') -> eq_ftp t t' && eq pot pot'
  | A.Arrow(t1,t2), A.Arrow(t1',t2') -> eq_ftp t1 t1' && eq_ftp t2 t2'
  | A.VarT v, A.VarT v' -> v = v'
  | _, _ -> false;;

let rec mem_seen env seen a a' = match seen with
    (b,b')::seen' ->
      if b = a && b' = a' then true
      else if b = a' && b' = a
      then true
      else mem_seen env seen' a a'
  | [] -> false;;

(* eq_tp env con seen A A' = true if (A = A'), defined coinductively *)
let rec eq_tp' env seen a a' =
  if !F.verbosity >= 3
  then print_string ("comparing " ^ PP.pp_tp env a ^ "\nand\n" ^ PP.pp_tp env a' ^ "\n")
  else ()
  ; eq_tp env seen a a'

and eq_tp env seen tp tp' = match tp, tp' with
    A.Plus(choice), A.Plus(choice') ->
      eq_choice env seen choice choice'
  | A.With(choice), A.With(choice') ->
      eq_choice env seen choice choice'
  | A.PPlus(pchoice), A.PPlus(pchoice') ->
      eq_pchoice env seen pchoice pchoice'
  | A.PWith(pchoice), A.PWith(pchoice') ->
      eq_pchoice env seen pchoice pchoice'
  | A.Tensor(s,t,m), A.Tensor(s',t',m') ->
      eqmode m m' && eq_tp' env seen s s' && eq_tp' env seen t t'
  | A.Lolli(s,t,m), A.Lolli(s',t',m') ->
      eqmode m m' && eq_tp' env seen s s' && eq_tp' env seen t t'
  | A.One, A.One -> true

  | A.PayPot(pot,a), A.PayPot(pot',a') ->
      eq pot pot' && eq_tp' env seen a a'
  | A.GetPot(pot,a), A.GetPot(pot',a') ->
      eq pot pot' && eq_tp' env seen a a'

  | A.Up(a), A.Up(a') ->
      eq_tp' env seen a a'
  | A.Down(a), A.Down(a') ->
      eq_tp' env seen a a'

  | A.FArrow(t,a), A.FArrow(t',a') ->
      eq_ftp t t' && eq_tp' env seen a a'
  | A.FProduct(t,a), A.FProduct(t',a') ->
      eq_ftp t t' && eq_tp' env seen a a'

  | A.TpName(a), A.TpName(a') ->
      eq_name_name env seen a a' (* coinductive type equality *)
  | A.TpName(a), a' ->
      eq_tp' env seen (A.expd_tp env a) a'
  | a, A.TpName(a') ->
      eq_tp' env seen a (A.expd_tp env a')

  | _a, _a' -> false

and eq_pchoice env seen pcs pcs' = match pcs, pcs' with
    [], [] -> true
  | (l,pr,a)::pchoice, (l',pr',a')::pchoice' ->
      l = l' && eq pr pr' && eq_tp' env seen a a'
      && eq_pchoice env seen pchoice pchoice'
  | _pcs, [] -> false
  | [], _pcs' -> false

and eq_choice env seen cs cs' = match cs, cs' with
    [], [] -> true
  | (l,a)::choice, (l',a')::choice' -> (* order must be equal *)
      l = l' && eq_tp' env seen a a'
      && eq_choice env seen choice choice'
  | _cs, [] -> false
  | [], _cs' -> false

and eq_name_name env seen a a' =
  if mem_seen env seen a a' then true
  else eq_tp' env ((a,a')::seen) (A.expd_tp env a) (A.expd_tp env a');;

let eqtp env tp tp' = eq_tp' env [] tp tp';; 

(* ******************* *)
(* Subtyping Algorithm *)
(* ******************* *)

let rec sub_tp' env seen a a' =
  if !F.verbosity >= 3
  then print_string ("comparing " ^ PP.pp_tp env a ^ " and " ^ PP.pp_tp env a' ^ "\n")
  else ()
  ; sub_tp env seen a a'

and sub_tp env seen tp tp' = match tp, tp' with
    A.Plus(choice), A.Plus(choice') ->
      sub_ichoice env seen choice choice'
  | A.With(choice), A.With(choice') ->
      sub_echoice env seen choice choice'
  | A.PPlus(pchoice), A.PPlus(pchoice') ->
      sub_pichoice env seen pchoice pchoice'
  | A.PWith(pchoice), A.PWith(pchoice') ->
      sub_pechoice env seen pchoice pchoice'
  | A.Tensor(s,t,m), A.Tensor(s',t',m') ->
      eqmode m m' && sub_tp' env seen s s' && sub_tp' env seen t t'
  | A.Lolli(s,t,m), A.Lolli(s',t',m') ->
      eqmode m m' && sub_tp' env seen s' s && sub_tp' env seen t t'
  | A.One, A.One -> true

  | A.PayPot(pot,a), A.PayPot(pot',a') ->
      eq pot pot' && sub_tp' env seen a a'
  | A.GetPot(pot,a), A.GetPot(pot',a') ->
      eq pot pot' && sub_tp' env seen a a'

  | A.Up(a), A.Up(a') ->
      sub_tp' env seen a a'
  | A.Down(a), A.Down(a') ->
      sub_tp' env seen a a'

  | A.FArrow(t,a), A.FArrow(t',a') ->
      eq_ftp t t' && sub_tp' env seen a a'
  | A.FProduct(t,a), A.FProduct(t',a') ->
      eq_ftp t t' && sub_tp' env seen a a'

  | A.TpName(a), A.TpName(a') ->
      sub_name_name env seen a a' (* coinductive subtyping *)
  | A.TpName(a), a' ->
      sub_tp' env seen (A.expd_tp env a) a'
  | a, A.TpName(a') ->
      sub_tp' env seen a (A.expd_tp env a')

  | _a, _a' -> false

and sub_pichoice env seen pcs pcs' = match pcs with
    [] -> true
  | (l,pr,a)::pchoice -> 
      let lab_match = List.find_all (fun (l', _, _) -> l = l') pcs' in
      if (List.length lab_match != 1)
      then false
      else let (_, pr', a') = List.nth lab_match 0 in 
      eq pr pr' && sub_tp' env seen a a' && sub_pichoice env seen pchoice pcs'

and sub_ichoice env seen cs cs' = match cs with
    [] -> true
  | (l,a)::choice -> 
      let lab_match = List.find_all (fun (l', _) -> l = l') cs' in
      if (List.length lab_match != 1)
      then false
      else let (_, a') = List.nth lab_match 0 in 
      sub_tp' env seen a a' && sub_ichoice env seen choice cs'

and sub_pechoice env seen pcs pcs' = match pcs' with
    [] -> true
  | (l',pr',a')::pchoice' -> 
      let lab_match = List.find_all (fun (l, _, _) -> l = l') pcs in
      if (List.length lab_match != 1)
      then false
      else let (_, pr, a) = List.nth lab_match 0 in 
      eq pr pr' && sub_tp' env seen a a' && sub_pechoice env seen pcs pchoice'

and sub_echoice env seen cs cs' = match cs' with
    [] -> true
  | (l',a')::choice' -> 
      let lab_match = List.find_all (fun (l, _) -> l = l') cs in
      if (List.length lab_match != 1)
      then false
      else let (_, a) = List.nth lab_match 0 in 
      sub_tp' env seen a a' && sub_echoice env seen cs choice'

and sub_name_name env seen a a' =
  if mem_seen env seen a a' then true
  else sub_tp' env ((a,a')::seen) (A.expd_tp env a) (A.expd_tp env a');;

let subtp env tp tp' = sub_tp' env [] tp tp';;

(*
  Sub-Synchronizing Session Types
  Purely linear types are always sub-synchronizing
*)
let rec ssync env seen tp c_opt ext =
  let _ = if !F.verbosity >= 3
          then match c_opt with
              None -> ()
            | Some c -> print_string ("checking ssync:\n" ^ PP.pp_tp env tp ^ "\nwith\n" ^ PP.pp_tp env c ^ "\n")
          else ()
  in
  match tp with
      A.Plus(choice) -> ssync_choices env seen choice c_opt ext
    | A.With(choice) -> ssync_choices env seen choice c_opt ext
    | A.PPlus(choice) -> ssync_pchoices env seen choice c_opt ext
    | A.PWith(choice) -> ssync_pchoices env seen choice c_opt ext
    | A.Tensor(_a,b,_m) -> ssync env seen b c_opt ext
    | A.Lolli(_a,b,_m) -> ssync env seen b c_opt ext
    | A.One ->
        begin
          match c_opt with
              None -> ()
            | Some _ -> error ext ("type not sub-synchronizing")
        end
    | A.PayPot(_pot,a) -> ssync env seen a c_opt ext
    | A.GetPot(_pot,a) -> ssync env seen a c_opt ext
    | A.TpName(v) ->
        begin
          if List.exists (fun x -> x = v) seen
          then ()
          else ssync env (v::seen) (A.expd_tp env v) c_opt ext
        end
    | A.Up(a) -> ssync env seen a (Some tp) ext
    | A.Down(a) ->
        begin
          match c_opt with
              None -> error ext ("type not sub-synchronizing")
            | Some c -> 
                if subtp env a c
                then ssync env seen a None ext
                else error ext ("type not sub-synchronizing")
        end
    | A.FArrow(_t,a) -> ssync env seen a c_opt ext
    | A.FProduct(_t,a) -> ssync env seen a c_opt ext

and ssync_pchoices env seen cs c ext = match cs with
    (_l,_pot,a)::cs' -> ssync env seen a c ext ; ssync_pchoices env seen cs' c ext
  | [] -> ()

and ssync_choices env seen cs c ext = match cs with
    (_l,a)::cs' -> ssync env seen a c ext ; ssync_choices env seen cs' c ext
  | [] -> ();;

let ssync_tp env tp ext = ssync env [] tp None ext;;

(*************************************)
(* Type checking process expressions *)
(*************************************)

exception UnknownTypeError;;

let chan_of (c, _tp) = c
let tp_of (_c, tp) = tp;;
let name_of (_s,c,_m) = c;;

let eq_str (s1,_c1,_m1) (s2,_c2,_m2) = s1 = s2;;

let eq_name (_s1,c1,_m1) (_s2,c2,_m2) = c1 = c2;;

let eq_mode (_s1,_c1,m1) (_s2,_c2,m2) = eqmode m1 m2;;

let eq_chan c1 c2 =
  eq_name c1 c2 && eq_mode c1 c2;;

let rec checktp c delta = match delta with
    [] -> false
  | (x,_t)::delta' ->
      if eq_name x c then true
      else checktp c delta';;

let rec checkftp v odelta = match odelta with
    [] -> false
  | (A.STyped _)::odelta' -> checkftp v odelta'
  | (A.Functional (w,_t))::odelta' ->
      if w = v then true
      else checkftp v odelta';;

let check_ftp v delta =
  let {A.shared = _sdelta ; A.linear = _ldelta ; A.ordered = odelta} = delta in
  checkftp v odelta;;

let check_tp c delta =
  let {A.shared = sdelta ; A.linear = ldelta ; A.ordered = _odelta} = delta in
  checktp c sdelta || checktp c ldelta;;

let check_stp c delta =
  let {A.shared = sdelta ; A.linear = _ldelta ; A.ordered = _odelta} = delta in
    checktp c sdelta;;

let check_ltp c delta =
  let {A.shared = _sdelta ; A.linear = ldelta ; A.ordered = _odelta} = delta in
    checktp c ldelta;;

let rec pure delta = match delta with
    [] -> true
  | (c,_t)::delta' ->
      if not (mode_P c)
      then false
      else pure delta';;

let purelin delta =
  let {A.shared = _sdelta ; A.linear = ldelta ; A.ordered = _odelta} = delta in
  pure ldelta;;

(* must check for existence first *)
let rec findtp c delta ext = match delta with
    [] -> raise UnknownTypeError
  | (x,t)::delta' ->
      if eq_chan x c then t
      else findtp c delta' ext;;

let rec findftp v delta = match delta with
    [] -> raise UnknownTypeError
  | (A.Functional (w,t))::delta' ->
      if v = w then t
      else findftp v delta'
  | (A.STyped _)::delta' ->
      findftp v delta';;

let find_ftp v delta =
  let {A.shared = _sdelta ; A.linear = _ldelta ; A.ordered = odelta} = delta in
  findftp v odelta;;

let find_stp c delta ext =
  let {A.shared = sdelta ; A.linear = _ldelta ; A.ordered = _odelta} = delta in
  if not (mode_S c)
  then error ext ("mode of channel " ^ PP.pp_chan c ^ " not S")
  else findtp c sdelta ext;;

let find_ltp c delta ext =
  let {A.shared = _sdelta ; A.linear = ldelta ; A.ordered = _odelta} = delta in
  if not (mode_lin c)
  then E.error_mode_shared_comm c ext
  else findtp c ldelta ext;;

let rec removetp x delta = match delta with
    [] -> []
  | (y,t)::delta' ->
      if eq_name x y
      then delta'
      else (y,t)::(removetp x delta');;

let rec removeotp x odelta = match odelta with
    [] -> []
  | a::odelta' ->
      match a with
          A.Functional(v,t) -> (A.Functional(v,t))::(removeotp x odelta')
        | A.STyped (y,t) ->
            if eq_name x y
            then odelta'
            else (A.STyped(y,t))::(removeotp x odelta');;

let remove_tp x delta =
  let {A.shared = sdelta ; A.linear = ldelta ; A.ordered = odelta} = delta in
  {A.shared = removetp x sdelta ; A.linear = removetp x ldelta ; A.ordered = removeotp x odelta};;

let add_chan env (x,a) delta =
  let {A.shared = sdelta ; A.linear = ldelta ; A.ordered = odelta} = delta in
  if A.is_shared env a
  then {A.shared = (x,a)::sdelta ; A.linear = ldelta ; A.ordered = (A.STyped (x,a))::odelta}
  else {A.shared = sdelta ; A.linear = (x,a)::ldelta ; A.ordered = (A.STyped (x,a))::odelta};;

let add_var (v,t) delta =
  let {A.shared = sdelta ; A.linear = ldelta ; A.ordered = odelta} = delta in
  let odelta' = (A.Functional(v,t))::odelta in
  {A.shared = sdelta ; A.linear = ldelta ; A.ordered = odelta'};;

let update_tp env x t delta =
  let delta = remove_tp x delta in
  add_chan env (x,t) delta;;

let rec match_ctx env sig_ctx ctx delta sig_len len ext = match sig_ctx, ctx with
    (A.STyped (sc,st))::sig_ctx', (A.STArg c)::ctx' ->
      begin
        if not (check_tp c delta)
        then error ext ("unknown or duplicate variable: " ^ PP.pp_chan c)
        else if not (eq_mode sc c)
        then E.error_mode_mismatch (sc, c) ext
        else
          let {A.shared = sdelta ; A.linear = _ldelta ; A.ordered = _odelta} = delta in
          if checktp c sdelta
          then
            begin
              let t = find_stp c delta ext in
              (* "subtype is sufficient with subsync types" *)
              if subtp env t st
              then match_ctx env sig_ctx' ctx' delta sig_len len ext
              else error ext ("shared type mismatch: type of " ^ PP.pp_chan c ^ " : " ^ PP.pp_tp_compact env t ^
                          " does not match type in declaration: " ^ PP.pp_tp_compact env st)
            end
          else
            begin
              let t = find_ltp c delta ext in
              (* "subtype is sufficient with subsync types" *)
              if subtp env t st
              then match_ctx env sig_ctx' ctx' (remove_tp c delta) sig_len len ext
              else error ext ("linear type mismatch: type of " ^ PP.pp_chan c ^ " : " ^ PP.pp_tp_compact env t ^
                          " does not match type in declaration: " ^ PP.pp_tp_compact env st)
            end
      end
  | (A.Functional (_sv,st))::sig_ctx', (A.FArg (A.Var v))::ctx' ->
      begin
        if not (check_ftp v delta)
        then error ext ("unknown or duplicate variable: " ^ v)
        else
          let t = find_ftp v delta in
          if eq_ftp st t
          then match_ctx env sig_ctx' ctx' delta sig_len len ext
          else error ext ("functional type mismatch: type of " ^ v ^ " : " ^ PP.pp_ftp_simple t ^
                      " does not match type in declaration: " ^ PP.pp_ftp_simple st)
      end
  | [], [] -> delta
  | _, _ -> error ext ("process defined with " ^ string_of_int sig_len ^
            " arguments but called with " ^ string_of_int len ^ " arguments");;

let join delta =
  let {A.shared = _sdelta ; A.linear = _ldelta ; A.ordered = odelta} = delta in
  odelta;;

let rec lookup_var x odelta = match odelta with
    A.Functional(y,t)::odelta' -> if x = y then Some t else lookup_var x odelta'
  | A.STyped _::odelta' -> lookup_var x odelta'
  | [] -> None;;

let lookup_ftp x delta ext =
  let {A.shared = _sdelta; A.linear = _ldelta; A.ordered = odelta} = delta in
  match lookup_var x odelta with
      None -> error ext ("unknown variable " ^ x)
    | Some t -> t;;

let min_potential pot pot' =
  if not (eq pot pot')
  then raise UnknownTypeError
  else pot;;

let rec min_tp t t' = match t, t' with
    A.Integer, A.Integer -> A.Integer
  | A.Boolean, A.Boolean -> A.Boolean
  | A.Arrow(t1,t2), A.Arrow(t1',t2') -> A.Arrow(min_tp t1 t1', min_tp t2 t2')
  | A.ListTP(t1,pot), A.ListTP(t1',pot') -> A.ListTP(min_tp t1 t1', min_potential pot pot')
  | _t, _t' -> raise UnknownTypeError;;

let rec min_delta odelta1 odelta2 = match odelta1 with
    A.STyped(x,a)::odelta1' -> A.STyped(x,a)::(min_delta odelta1' odelta2)
  | A.Functional(v,t1)::odelta1' ->
      begin
        match lookup_var v odelta2 with
            None -> min_delta odelta1' odelta2
          | Some t2 -> A.Functional(v, min_tp t1 t2)::(min_delta odelta1' odelta2)
      end
  | [] -> [];;

let min_pot (delta1, pot1) (delta2, pot2) =
  let {A.shared = sdelta1 ; A.linear = ldelta1 ; A.ordered = odelta1} = delta1 in
  let {A.shared = _sdelta2 ; A.linear = _ldelta2 ; A.ordered = odelta2} = delta2 in
  ({A.shared = sdelta1; A.linear = ldelta1; A.ordered = min_delta odelta1 odelta2}, min_potential pot1 pot2);;

let rec removevar x odelta = match odelta with
    [] -> []
  | A.Functional (y,t)::odelta' -> if x = y then odelta' else (A.Functional (y,t))::(removevar x odelta')
  | A.STyped(c,a)::odelta' -> (A.STyped (c,a))::(removevar x odelta');;

let remove_var x delta =
  {A.shared = delta.A.shared; A.linear = delta.A.linear; A.ordered = removevar x delta.A.ordered};;

let rec consume_pot tp = match tp with
    A.Integer | A.Boolean | A.String | A.Address | A.VarT _ -> tp
  | A.ListTP(t,_pot) -> A.ListTP(consume_pot t, A.Arith (R.Float(0.)))
  | A.Arrow(t1,t2) -> A.Arrow(consume_pot t1, consume_pot t2);;

let rec consumevar x odelta = match odelta with
    [] -> []
  | A.STyped(c,a)::odelta' -> (A.STyped(c,a))::(consumevar x odelta')
  | A.Functional(y,t)::odelta' ->
      if x = y
      then (A.Functional(y,consume_pot t))::odelta'
      else (A.Functional(y,t))::odelta';;

let consume x delta =
  {A.shared = delta.A.shared; A.linear = delta.A.linear; A.ordered = consumevar x delta.A.ordered};;

let rec consify l = match l with
    [] -> A.ListE([])
  | e::es ->
      let d = e.A.func_data in
      A.Cons(e, {A.func_data = d; A.func_structure = consify es});;
  
let tc_printable_arg arg t_exp_opt delta ext =
  match arg with
      A.FArg (A.Var v) ->
        begin
          if not (check_ftp v delta)
          then error ext ("unknown or duplicate variable: " ^ v)
          else
          let t = find_ftp v delta in
          match t_exp_opt with
              None -> error ext ("invalid type of " ^ v ^ ", expected: channel, found: " ^ PP.pp_ftp_simple t)
            | Some t_exp ->
                if eq_ftp t_exp t
                then ()
                else error ext ("invalid type of " ^ v ^
                                ", expected: " ^ PP.pp_ftp_simple t_exp ^ ", found: " ^ PP.pp_ftp_simple t)
        end
    | A.FArg _e -> raise UnknownTypeError
    | A.STArg c ->
        if not (check_tp c delta)
        then error ext ("unknown or duplicate variable: " ^ PP.pp_chan c)
        else
        match t_exp_opt with
            None -> ()
          | Some t_exp -> error ext ("invalid type of " ^ PP.pp_chan c ^ ", expected: " ^
                                     PP.pp_ftp_simple t_exp ^ ", found: channel");;

let rec check_printable_list delta ext plist arglist plen arglen =
  match plist, arglist with
      [], [] -> ()
    | A.Word(_)::ps, _ -> check_printable_list delta ext ps arglist plen arglen
    | A.PNewline::ps, _ -> check_printable_list delta ext ps arglist plen arglen
    | A.PInt::ps, arg::args ->
        let () = tc_printable_arg arg (Some A.Integer) delta ext in
        check_printable_list delta ext ps args plen arglen
    | A.PBool::ps, arg::args ->
        let () = tc_printable_arg arg (Some A.Boolean) delta ext in
        check_printable_list delta ext ps args plen arglen
    | A.PStr::ps, arg::args ->
        let () = tc_printable_arg arg (Some A.String) delta ext in
        check_printable_list delta ext ps args plen arglen
    | A.PAddr::ps, arg::args ->
        let () = tc_printable_arg arg (Some A.Address) delta ext in
        check_printable_list delta ext ps args plen arglen
    | A.PChan::ps, arg::args ->
        let () = tc_printable_arg arg None delta ext in
        check_printable_list delta ext ps args plen arglen
    | _, _ ->
        error ext ("print string expects " ^ string_of_int plen ^
                  " arguments but called with " ^ string_of_int arglen ^ " arguments");;

let is_argtype p = match p with
    A.Word _ | A.PNewline -> false
  | A.PInt | A.PBool | A.PStr | A.PAddr | A.PChan -> true;;

let filter_args l = List.filter (fun p -> is_argtype p) l;;

let pr_zero = A.Arith(R.Float 0.0);;
let pr_one = A.Arith(R.Float 1.0);;

let rec plabprob k pchoices ext = match pchoices with
    [] -> ()
  | (l,prob,_a)::pchoices' ->
      if k = l
      then
        begin
          if eq prob pr_one
          then plabprob k pchoices' ext
          else error ext ("prob. mismatch of label " ^ l ^ ": expected {1}, found: " ^ PP.pp_prob prob)
        end
      else
        begin
          if eq prob pr_zero
          then plabprob k pchoices' ext
          else error ext ("prob. mismatch of label " ^ l ^ ": expected {0}, found: " ^ PP.pp_prob prob)
        end

let rec in_ctx c xs = match xs with
    [] -> false
  | x::xs' ->
      match x with
          A.STArg c' -> eq_name c' c || in_ctx c xs'
        | A.FArg _ -> in_ctx c xs';;

let rec find_sigtp ctx xs c = match ctx, xs with
    A.STyped (_sc,st)::ctx', A.STArg c'::xs' ->
      if eq_name c' c
      then st
      else find_sigtp ctx' xs' c
  | A.Functional _::ctx', A.FArg _::xs' -> find_sigtp ctx' xs' c
  | _, _ -> raise UnknownTypeError;;

let get_typeL env f xs c =
  match A.lookup_expdec env f with
      None -> raise UnknownTypeError
    | Some (ctx,_lpot,_xa,_mdef) ->
        let ctx = join ctx in
        find_sigtp ctx xs c;;

let get_typeR env f =
  match A.lookup_expdec env f with
      None -> raise UnknownTypeError
    | Some (_ctx,_lpot,(_x,a),_mdef) -> a;;

let rec gen_pchoices pchoices k = match pchoices with
    [] -> []
  | (l,_pr,a)::pchoices' ->
      if k = l
      then (l,pr_one,a)::(gen_pchoices pchoices' k)
      else (l,pr_zero,a)::(gen_pchoices pchoices' k);;

let gen_tp k a = match a with
    A.PPlus(pchoices) -> A.PPlus(gen_pchoices pchoices k)
  | A.PWith(pchoices) -> A.PWith(gen_pchoices pchoices k)
  | _a -> raise UnknownTypeError;;

let add pr1 pr2 = match pr1, pr2 with
    A.Arith p1, A.Arith p2 -> A.Arith(R.Add(p1,p2))
  | A.Star, _ -> A.Star
  | _, A.Star -> A.Star;;

let mult pr1 pr2 = match pr1, pr2 with
    A.Arith p1, A.Arith p2 -> A.Arith(R.Mult(p1,p2))
  | A.Star, _ -> A.Star
  | _, A.Star -> A.Star;;

let rec add_choices env x pcs pcs' ext = match pcs, pcs' with
    [], [] -> []
  | (l,prob,a)::pcstl, (l',prob',a')::pcstl' ->
      if l <> l'
      then error ext ("label mismatch in " ^ PP.pp_chan x ^ ": " ^ l ^ " <> " ^ l')
      else if not (subtp env a a')
      then error ext ("type mismatch in " ^ PP.pp_chan x ^ ": " ^ PP.pp_tp_compact env a ^ " <> " ^ PP.pp_tp_compact env a')
      else (l, add prob prob', a)::(add_choices env x pcstl pcstl' ext)
  | _, _ -> error ext ("prob. mismatch in type of " ^ PP.pp_chan x ^ ": unequal lengths");;

let rec add_ptypes env c a a' ext = match a, a' with
    _, A.TpName(v) -> add_ptypes env c a (A.expd_tp env v) ext
  | A.TpName(v), _ -> add_ptypes env c (A.expd_tp env v) a' ext
  | A.PPlus(pcs), A.PPlus(pcs') -> A.PPlus(add_choices env c pcs pcs' ext)
  | A.PWith(pcs), A.PWith(pcs') -> A.PWith(add_choices env c pcs pcs' ext)
  | _, _ -> a';;

let rec mult_choices env pr pcs = match pcs with
    [] -> []
  | (l,prob,a)::pcstl -> (l, mult pr prob, a)::(mult_choices env pr pcstl);;

let rec mult_ptype env pr a = match a with
    A.TpName(v) -> mult_ptype env pr (A.expd_tp env v)
  | A.PPlus(pcs) -> A.PPlus(mult_choices env pr pcs)
  | A.PWith(pcs) -> A.PWith(mult_choices env pr pcs)
  | _ -> a

let rec weighted_psum env c al ext = match al with
    [] -> raise UnknownTypeError
  | [(_l,pr,a)] -> mult_ptype env pr a
  | (_l,pr,a)::al' ->
      let a' = weighted_psum env c al' ext in
      let pra = mult_ptype env pr a in
      add_ptypes env c pra a' ext;;

let comp p =
  let one = R.Float(1.0) in
  R.Sub(one,p);;

let comp_star pr = match pr with
    A.Arith p -> A.Arith (comp p)
  | A.Star -> A.Star;;

let rec action env p c a = match p.A.st_structure with
    A.Fwd(_x,_y) -> a
  | A.Spawn(_x,f,xs,q) ->
      if in_ctx c xs
      then get_typeL env f xs c
      else action env q c a
  | A.ExpName(x,f,xs) ->
      if eq_name x c
      then get_typeR env f
      else get_typeL env f xs c
  | A.Lab(x,_k,q) ->
      if eq_name x c
      then a
      else action env q c a
  | A.Case(_x,branches) -> action_branches env branches c a
  | A.PLab(x,k,q) ->
      if eq_name x c
      then gen_tp k a
      else action env q c a
  | A.PCase(x,pbranches) ->
      if eq_name x c
      then a
      else weighted_psum env c (action_pbranches env pbranches c a) p.A.st_data
  | A.Flip(pr,q1,q2) -> weighted_psum env c (action_pbranches env [("HH", pr, q1); ("TT", comp_star pr, q2)] c a) p.A.st_data
  | A.Send(x,w,q) ->
      if eq_name x c || eq_name w c
      then a
      else action env q c a
  | A.Recv(x,_y,q) ->
      if eq_name x c
      then a
      else action env q c a
  | Close(_x) -> a
  | Wait(x,q) ->
      if eq_name x c
      then a
      else action env q c a
  | Work(_pot,q) -> action env q c a
  | Pay(x,_pot,q) ->
      if eq_name x c
      then a
      else action env q c a
  | Get(x,_pot,q) ->
      if eq_name x c
      then a
      else action env q c a
  | Acquire(x,_y,q) ->
      if eq_name x c
      then a
      else action env q c a
  | Accept(x,_y,q) ->
      if eq_name x c
      then a
      else action env q c a
  | Release(x,_y,q) ->
      if eq_name x c
      then a
      else action env q c a
  | Detach(x,_y,q) ->
      if eq_name x c
      then a
      else action env q c a
  | RecvF(x,_y,q) ->
      if eq_name x c
      then a
      else action env q c a
  | SendF(x,_m,q) ->
      if eq_name x c
      then a
      else action env q c a
  | Let(_x,_e,q) -> action env q c a
  | IfS(_e,q1,_q2) -> action env q1 c a
  | MakeChan(_x,_a,_n,q) -> action env q c a
  | Abort -> a
  | Print(_l,_args,q) -> action env q c a

and action_pbranches env pbranches c a = match pbranches with
    [] -> raise UnknownTypeError
  | [(l, pr, p)] -> [(l, pr, action env p c a)]
  | (l, pr, p)::pbranches' ->
      (l, pr, action env p c a)::(action_pbranches env pbranches' c a)

and action_branches env branches c a = match branches with
    [] -> raise UnknownTypeError
  | (_l,q)::_bs -> action env q c a

let rec gen_prob_tpL env p x a = match a with
    A.TpName(v) -> gen_prob_tpL env p x (A.expd_tp env v)
  (* TODO: do other types change at all? *)
  | a -> action env p x a;;

let rec gen_prob_tpR env p x a = match a with
    A.TpName(v) -> gen_prob_tpR env p x (A.expd_tp env v)
  (* TODO: do other types change at all? *)
  | a -> action env p x a;;

let eq_name_opt x_opt c = match x_opt with
    None -> false
  | Some x -> eq_name x c;;

let gen_prob_ctx env delta p x_opt =
  let {A.shared = sdelta; A.linear = ldelta; A.ordered = odelta} = delta in
  let gen_func (c,a) = (c, if eq_name_opt x_opt c then a else gen_prob_tpL env p c a) in
  let gen_ofunc arg = match arg with
                          A.Functional _ -> arg
                        | A.STyped (c,a) -> let (cl,al) = gen_func (c,a) in A.STyped(cl,al)
  in
  let sdeltaf = List.map gen_func sdelta in
  let ldeltaf = List.map gen_func ldelta in
  let odeltaf = List.map gen_ofunc odelta in
  {A.shared = sdeltaf; A.linear = ldeltaf; A.ordered = odeltaf};;

let rec label_errormsg env al = match al with
    [] -> ""
  | (l,pr,a)::al' -> l ^ PP.pp_prob pr ^ " : " ^ PP.pp_tp_compact env a ^ "\n" ^ label_errormsg env al';;

let prob_error env x a a' al ext =
  let a'str = PP.pp_tp_compact env a' in
  let lstr = label_errormsg env al in
  error ext ("prob. mismatch in type of " ^ PP.pp_chan x ^ ", expected: " ^ PP.pp_tp_compact env a ^ "\nfound: " ^ a'str ^ "\ndue to branches:\n" ^ lstr);;

let match_probs env x a al ext =
  let a' = weighted_psum env x al ext in
  if subtp env a a'
  then ()
  else prob_error env x a a' al ext

let rec extract x delta = match delta with
    [] -> raise UnknownTypeError
  | (y,t)::ys ->
      if eq_name x y
      then t
      else extract x ys;;

let rec extract_ord x odelta = match odelta with
    [] -> raise UnknownTypeError
  | A.Functional _::ys -> extract_ord x ys
  | A.STyped(y,t)::ys ->
      if eq_name x y
      then t
      else extract_ord x ys;;

let sextract x delta =
  let {A.shared = sdelta; A.linear = _ldelta; A.ordered = _odelta} = delta in
  extract x sdelta;;

let lextract x delta =
  let {A.shared = _sdelta; A.linear = ldelta; A.ordered = _odelta} = delta in
  extract x ldelta;;

let oextract x delta =
  let {A.shared = _sdelta; A.linear = _ldelta; A.ordered = odelta} = delta in
  extract_ord x odelta;;

let sextractl x deltal = List.map (fun (l,prob,delta) -> (l,prob,sextract x delta)) deltal;;

let lextractl x deltal = List.map (fun (l,prob,delta) -> (l,prob,lextract x delta)) deltal;;

let oextractl x deltal = List.map (fun (l,prob,delta) -> (l,prob,oextract x delta)) deltal;;

let match_probs_ctx env delta deltal ext =
  let {A.shared = sdelta; A.linear = ldelta; A.ordered = odelta} = delta in
  let match_sfunc (x,a) = match_probs env x a (sextractl x deltal) ext in
  let match_lfunc (x,a) = match_probs env x a (lextractl x deltal) ext in
  let match_ofunc arg = match arg with
                            A.Functional _ -> ()
                          | A.STyped (x,a) -> match_probs env x a (oextractl x deltal) ext
  in
  let _s = List.map match_sfunc sdelta in
  let _l = List.map match_lfunc ldelta in
  let _o = List.map match_ofunc odelta in
  ();;


type probmode = Prob | NonProb;;

let prob prmode = match prmode with
    Prob -> true
  | NonProb -> false;;

let faug exp data = {A.func_structure = exp; A.func_data = data};;
let staug exp data = {A.st_structure = exp; A.st_data = data};;

let create_app l en =
  let {A.func_structure = le; A.func_data = lext} = l in
  match le with
      A.App(ale) -> A.App(ale @ [en])
    | a -> A.App([faug a lext ; en]);;

let pot_branch pr = match pr with
    A.Star -> A.Star
  | A.Arith _pr -> A.Arith (I.fresh_potvar ());;
(*******************************)
(* Main Type Checking Function *)
(*******************************)

let rec check_fexp_simple' trace env delta pot (e : A.parsed_expr) tp ext mode isSend prmode =
  begin
    if trace
    then print_string ("Checking: [" ^ PP.pp_mode mode ^ "] : " ^  PP.pp_fexp env 0 (e.A.func_structure) ^ " : "
                          ^ PP.pp_ftp_simple tp ^ "\n")
    else ()
  end
  ; check_fexp_simple trace env delta pot e tp ext mode isSend prmode

and synth_fexp_simple' trace env delta pot (e : A.parsed_expr) ext mode isSend prmode =
  begin
    if trace
    then print_string ("Synth: [" ^ PP.pp_mode mode ^ "] : " ^  PP.pp_fexp env 0 (e.A.func_structure) ^ "\n")
    else ()
  end
  ; synth_fexp_simple trace env delta pot e ext mode isSend prmode

and check_fexp_simple trace env delta pot (e : A.parsed_expr) tp ext mode isSend prmode = match (e.A.func_structure) with
    A.If(e1,e2,e3) ->
      begin
        let (delta1, pot1, e1') = check_fexp_simple' trace env delta pot e1 A.Boolean ext mode isSend prmode in
        let (delta2, pot2, e2') = check_fexp_simple' trace env delta1 pot1 e2 tp ext mode isSend prmode in
        let (delta3, pot3, e3' ) = check_fexp_simple' trace env delta1 pot1 e3 tp ext mode isSend prmode in
        let (delta', pot') = min_pot (delta2, pot2) (delta3, pot3) in
        (delta', pot', faug (A.If(e1', e2', e3')) e.A.func_data)
      end
  | A.LetIn(x,e1,e2) ->
      begin
        let (delta1, pot1, t, e1') = synth_fexp_simple trace env delta pot e1 ext mode isSend prmode in
        let (delta2, pot2, e2') = check_fexp_simple' trace env (add_var (x,t) delta1) pot1 e2 tp ext mode isSend prmode in
        (remove_var x delta2, pot2, faug (A.LetIn(x,e1',e2')) e.A.func_data)
      end
  | A.Bool(_) ->
      begin
        match tp with
            A.Boolean -> (delta, pot, e)
          | _ -> error (e.A.func_data) ("type mismatch of " ^ PP.pp_fexp env 0 (e.A.func_structure) ^ ": expected boolean, found: " ^ PP.pp_ftp_simple tp)
      end
  | A.Int(_) ->
      begin
        match tp with
            A.Integer -> (delta, pot, e)
          | _ -> error (e.A.func_data) ("type mismatch of " ^ PP.pp_fexp env 0 (e.A.func_structure) ^ ": expected integer, found: " ^ PP.pp_ftp_simple tp)
      end
  | A.Str(_) ->
      begin
        match tp with
            A.String -> (delta, pot, e)
          | _ -> error (e.A.func_data) ("type mismatch of " ^ PP.pp_fexp env 0 (e.A.func_structure) ^ ": expected string, found: " ^ PP.pp_ftp_simple tp)
      end
  | A.Addr(_) ->
      begin
        match tp with
            A.Address -> (delta, pot, e)
          | _ -> error (e.A.func_data) ("type mismatch of " ^ PP.pp_fexp env 0 (e.A.func_structure) ^ ": expected address, found: " ^ PP.pp_ftp_simple tp)
      end
  | A.Var(x) ->
      begin
        let t1 = lookup_ftp x delta (e.A.func_data) in
        if (eq_ftp tp t1)
        then
          let delta' = if isSend then consume x delta else delta in
          (delta', pot, e)
        else error (e.A.func_data) ("type mismatch of " ^ PP.pp_fexp env 0 (e.A.func_structure) ^ " : " ^ PP.pp_ftp_simple t1 ^ " <> " ^ PP.pp_ftp_simple tp)
      end
  | A.ListE(l) ->
      begin
        let cons_exp = consify l in
        check_fexp_simple' trace env delta pot {func_structure = cons_exp; func_data = e.A.func_data} tp ext mode isSend prmode
      end
  | A.Op(e1, op, e2) ->
      begin
        if not(eq_ftp tp A.Integer) then error (e.A.func_data) ("type mismatch of " ^ PP.pp_fexp env 0 (e.A.func_structure) ^ ", expected int, found: " ^ (PP.pp_ftp_simple tp))
        else
          let (delta1, pot1, e1') = check_fexp_simple' trace env delta pot e1 A.Integer ext mode isSend prmode in
          let (delta2, pot2, e2') = check_fexp_simple' trace env delta1 pot1 e2 A.Integer ext mode isSend prmode in
          (delta2, pot2, faug (A.Op(e1', op, e2')) e.A.func_data)
      end
  | A.CompOp(e1, cop, e2) ->
      begin
        if not(eq_ftp tp A.Boolean) then error (e.A.func_data) ("type mismatch of " ^ PP.pp_fexp env 0 (e.A.func_structure) ^ ", expected boolean, found: " ^ (PP.pp_ftp_simple tp))
        else
          let (delta1, pot1, e1') = check_fexp_simple' trace env delta pot e1 A.Integer ext mode isSend prmode in
          let (delta2, pot2, e2') = check_fexp_simple' trace env delta1 pot1 e2 A.Integer ext mode isSend prmode in
          (delta2, pot2, faug (A.CompOp(e1', cop, e2')) e.A.func_data)
      end
  | A.EqAddr(e1, e2) ->
      begin
        if not(eq_ftp tp A.Boolean) then error (e.A.func_data) ("type mismatch of " ^ PP.pp_fexp env 0 (e.A.func_structure) ^ ", expected boolean, found: " ^ (PP.pp_ftp_simple tp))
        else
          let (delta1, pot1, e1') = check_fexp_simple' trace env delta pot e1 A.Address ext mode isSend prmode in
          let (delta2, pot2, e2') = check_fexp_simple' trace env delta1 pot1 e2 A.Address ext mode isSend prmode in
          (delta2, pot2, faug (A.EqAddr(e1', e2')) e.A.func_data)
      end
  | A.RelOp(e1, rop, e2) ->
      begin
        if not(eq_ftp tp A.Boolean) then error (e.A.func_data) ("type mismatch of " ^ PP.pp_fexp env 0 (e.A.func_structure) ^ ", expected boolean, found: " ^ (PP.pp_ftp_simple tp))
        else
          let (delta1, pot1, e1') = check_fexp_simple' trace env delta pot e1 A.Boolean ext mode isSend prmode in
          let (delta2, pot2, e2') = check_fexp_simple' trace env delta1 pot1 e2 A.Boolean ext mode isSend prmode in
          (delta2, pot2, faug (A.RelOp(e1', rop, e2')) e.A.func_data)
      end
  | A.Cons(e1, e2) ->
      begin
        match tp with
          A.ListTP(t', pot') ->
          let (delta1, pot1, e1') = check_fexp_simple' trace env delta pot e1 t' ext mode isSend prmode in
          let (delta2, pot2, e2') = check_fexp_simple' trace env delta1 pot1 e2 tp ext mode isSend prmode in
          if not(ge pot2 pot') then error (e.A.func_data) ("insufficient potential for cons: " ^ PP.pp_fexp env 0 (e.A.func_structure) ^ " : " ^ pp_lt pot2 pot')
          else (delta2, minus pot2 pot', faug (A.Cons(e1', e2')) e.A.func_data)
        | _ -> error (e.A.func_data) ("type mismatch of " ^ PP.pp_fexp env 0 (e.A.func_structure) ^ ", expected list, found: " ^ PP.pp_ftp_simple tp)
      end
  | A.Match(e1, e2, x, xs, e3) ->
      begin
        let (delta1, pot1, t, e1') = synth_fexp_simple' trace env delta pot e1 ext mode isSend prmode in
        match t with
            A.ListTP(t', pot') ->
              let (delta2, pot2, e2') = check_fexp_simple' trace env delta1 pot1 e2 tp ext mode isSend prmode in
              let (delta3, pot3, e3') = check_fexp_simple' trace env (add_var (x, t') (add_var (xs, t) delta2)) (plus pot2 pot') e3 tp ext mode isSend prmode in
              (remove_var x (remove_var xs delta3), pot3, faug (A.Match(e1', e2', x, xs, e3')) e.A.func_data)
          | _ -> error (e.A.func_data) ("type mismatch of " ^ PP.pp_fexp env 0 e1.A.func_structure ^ ", expected list, found: " ^ PP.pp_ftp_simple t)
      end
  | A.Lambda(args, e0) ->
      begin
        match tp, args with
            A.Arrow(t1, t2), A.Single(x, _) ->
              let (delta1, pot1, e0') = check_fexp_simple' trace env (add_var (x, t1) delta) pot e0 t2 ext mode isSend prmode in
              (delta1, pot1, faug (A.Lambda(args, e0')) e.A.func_data)
          | A.Arrow(t1, t2), A.Curry((x, ext'), xs) -> check_fexp_simple' trace env (add_var (x, t1) delta) pot ({func_structure = A.Lambda(xs, e0); func_data = ext'}) t2 ext mode isSend prmode
          | _  -> error (e.A.func_data) ("type mismatch of " ^ PP.pp_fexp env 0 (e.A.func_structure) ^ ", expected arrow, found: " ^ PP.pp_ftp_simple tp)
      end
  | A.App(l) ->
      begin
        let (l0, en) = A.split_last l in
        let (delta1, pot1, t, l0') =
          begin
            if List.length l0 > 1
            then synth_fexp_simple' trace env delta pot ({func_structure = A.App(l0); func_data = e.A.func_data}) ext mode isSend prmode
            else synth_fexp_simple' trace env delta pot (List.hd l0) ext mode isSend prmode
          end
        in
        match t with
            A.Arrow(t1,t2) ->
              if not (eq_ftp t2 tp)
              then error (e.A.func_data) ("type mismatch of " ^ PP.pp_fexp env 0 (e.A.func_structure) ^ ", expected: " ^ PP.pp_ftp_simple tp ^ ", found: " ^ PP.pp_ftp_simple t2)
              else
                let (delta2, pot2, en') = check_fexp_simple' trace env delta1 pot1 en t1 ext mode isSend prmode in
                (delta2, pot2, faug (create_app l0' en') e.A.func_data)
          | _t -> error (e.A.func_data) ("type mismatch of " ^ PP.pp_fexp env 0 (A.App(l0)) ^ ", expected arrow, found: " ^ PP.pp_ftp_simple t)
      end
  | A.Tick(cpot,e1) ->
      begin
        if not (ge pot cpot)
        then error (e.A.func_data) ("insufficient potential to tick: " ^ pp_lt pot cpot)
        else
          let (delta1, pot1) = (delta, minus pot cpot) in
          let (delta2, pot2, e1') = check_fexp_simple' trace env delta1 pot1 e1 tp ext mode isSend prmode in
          (delta2, pot2, faug (A.Tick(cpot, e1')) e.A.func_data)
      end
  | A.GetTxnNum ->
      begin
        match tp with
            A.Integer -> (delta, pot, e)
          | _ -> error (e.A.func_data) ("type mismatch of " ^ PP.pp_fexp env 0 (e.A.func_structure) ^ ": expected integer, found: " ^ PP.pp_ftp_simple tp)
      end
  | A.GetTxnSender ->
      begin
        match tp with
            A.Address -> (delta, pot, e)
          | _ -> error (e.A.func_data) ("type mismatch of " ^ PP.pp_fexp env 0 (e.A.func_structure) ^ ": expected address, found: " ^ PP.pp_ftp_simple tp)
      end
  | A.Command _ -> raise UnknownTypeError

and synth_fexp_simple trace env delta pot (e : A.parsed_expr) ext mode isSend prmode = match (e.A.func_structure) with
    A.If(e1,e2,e3) ->
      begin
        let (delta1, pot1, e1') = check_fexp_simple' trace env delta pot e1 A.Boolean ext mode isSend prmode in
        let (delta2, pot2, t, e2') = synth_fexp_simple' trace env delta1 pot1 e2 ext mode isSend prmode in
        let (delta3, pot3, e3') = check_fexp_simple' trace env delta1 pot1 e3 t ext mode isSend prmode in
        let (odelta, opot) = min_pot (delta2, pot2) (delta3, pot3) in
        (odelta, opot, t, faug (A.If(e1', e2', e3')) e.A.func_data)
      end
  | A.LetIn _ -> error (e.A.func_data) ("cannot synthesize type of " ^ PP.pp_fexp env 0 (e.A.func_structure))
  | A.Bool _ -> (delta, pot, A.Boolean, e)
  | A.Int _ -> (delta, pot, A.Integer, e)
  | A.Str _ -> (delta, pot, A.String, e)
  | A.Addr _ -> (delta, pot, A.Address, e)
  | A.Var(x) ->
      begin
        let t = lookup_ftp x delta (e.A.func_data) in
        let delta' = if isSend then consume x delta else delta in
        (delta', pot, t, e)
      end
  | A.ListE(l) ->
      begin
        if List.length l = 0
        then error (e.A.func_data) ("cannot synthesize type of empty list: " ^ PP.pp_fexp env 0 (e.A.func_structure))
        else
          let cons_exp = consify l in
          synth_fexp_simple' trace env delta pot {func_structure = cons_exp; func_data = e.A.func_data} ext mode isSend prmode
      end
  | A.Cons(e1,e2) ->
      begin
        let (delta2, pot2, tplist, e2') = synth_fexp_simple' trace env delta pot e2 ext mode isSend prmode in
        match tplist with
            A.ListTP(tp,_pot) ->
              let (delta1, pot1, e1') = check_fexp_simple' trace env delta2 pot2 e1 tp ext mode isSend prmode in
              (delta1, pot1, tplist, faug (A.Cons(e1', e2')) e.A.func_data)
          | _t -> error (e.A.func_data) ("type of " ^ PP.pp_fexp env 0 e2.A.func_structure ^ " not a list")
      end
  | A.Match(e1, e2, x, xs, e3) ->
      begin
        let (delta1, pot1, t, e1') = synth_fexp_simple' trace env delta pot e1 ext mode isSend prmode in
        match t with
            A.ListTP(t', pot') ->
              let (delta2, pot2, tp, e2') = synth_fexp_simple' trace env delta1 pot1 e2 ext mode isSend prmode in
              let (delta3, pot3, e3') = check_fexp_simple' trace env (add_var (x, t') (add_var (xs, t) delta2)) (plus pot2 pot') e3 tp ext mode isSend prmode in
              (remove_var x (remove_var xs delta3), pot3, tp, faug (A.Match(e1', e2', x, xs, e3')) e.A.func_data)
          | _ -> error (e.A.func_data) ("type mismatch of " ^ PP.pp_fexp env 0 e1.A.func_structure ^ ", expected list, found: " ^ PP.pp_ftp_simple t)
      end
  | A.Lambda _ -> error (e.A.func_data) ("cannot synthesize type of " ^ PP.pp_fexp env 0 (e.A.func_structure))
  | A.Op(e1, op, e2) ->
      begin
        let (delta1, pot1, e1') = check_fexp_simple' trace env delta pot e1 A.Integer ext mode isSend prmode in
        let (delta2, pot2, e2') = check_fexp_simple' trace env delta1 pot1 e2 A.Integer ext mode isSend prmode in
        (delta2, pot2, A.Integer, faug (A.Op(e1', op, e2')) e.A.func_data)
      end
  | A.CompOp(e1, cop, e2) ->
      begin
        let (delta1, pot1, e1') = check_fexp_simple' trace env delta pot e1 A.Integer ext mode isSend prmode in
        let (delta2, pot2, e2') = check_fexp_simple' trace env delta1 pot1 e2 A.Integer ext mode isSend prmode in
        (delta2, pot2, A.Boolean, faug (A.CompOp(e1', cop, e2')) e.A.func_data)
      end
  | A.EqAddr(e1, e2) ->
      begin
        let (delta1, pot1, e1') = check_fexp_simple' trace env delta pot e1 A.Address ext mode isSend prmode in
        let (delta2, pot2, e2') = check_fexp_simple' trace env delta1 pot1 e2 A.Address ext mode isSend prmode in
        (delta2, pot2, A.Boolean, faug (A.EqAddr(e1', e2')) e.A.func_data)
      end
  | A.RelOp(e1, rop, e2) ->
      begin
        let (delta1, pot1, e1') = check_fexp_simple' trace env delta pot e1 A.Boolean ext mode isSend prmode in
        let (delta2, pot2, e2') = check_fexp_simple' trace env delta1 pot1 e2 A.Boolean ext mode isSend prmode in
        (delta2, pot2, A.Boolean, faug (A.RelOp(e1', rop, e2')) e.A.func_data)
      end
  | A.App(l) ->
      begin
        let (l0, en) = A.split_last l in
        let (delta1, pot1, t, l0') =
          begin
            if List.length l0 > 1
            then synth_fexp_simple' trace env delta pot ({func_structure=A.App(l0); func_data = (e.A.func_data)}) ext mode isSend prmode
            else synth_fexp_simple' trace env delta pot ((List.hd l0)) ext mode isSend prmode
          end
        in
        match t with
            A.Arrow(t1,t2) ->
              let (delta2, pot2, en') = check_fexp_simple' trace env delta1 pot1 en t1 ext mode isSend prmode in
              (delta2, pot2, t2, faug (create_app l0' en') e.A.func_data)
          | _t -> error (e.A.func_data) ("type mismatch of " ^ PP.pp_fexp env 0 (A.App(l0)) ^ ", expected arrow, found: " ^ PP.pp_ftp_simple t)
      end
  | A.Tick(cpot,e1) ->
      begin
        if not (ge pot cpot)
        then error (e.A.func_data) ("insufficient potential to tick: " ^ pp_lt pot cpot)
        else
          let (delta1, pot1) = (delta, minus pot cpot) in
          let (delta2, pot2, t, e1') = synth_fexp_simple' trace env delta1 pot1 e1 ext mode isSend prmode in
          (delta2, pot2, t, faug (A.Tick(cpot, e1')) e.A.func_data)
      end
  | A.GetTxnNum -> (delta, pot, A.Integer, e)
  | A.GetTxnSender -> (delta, pot, A.Address, e)
  | A.Command _ -> error (e.A.func_data) ("cannot synthesize type of " ^ PP.pp_fexp env 0 (e.A.func_structure))


and checkfexp trace env delta pot e zc ext mode prmode = match e.A.func_structure with
    A.Command(p) -> faug (A.Command(check_exp' trace env delta pot p zc ext mode prmode)) e.A.func_data
  | _ -> error (e.A.func_data) ("only command allowed at outermost declaration")

and check_exp' trace env delta pot p zc ext mode prmode =
  begin
    if trace
    then print_string ("Checking [" ^ PP.pp_mode mode ^ "] : " ^  PP.pp_exp_prefix (p.A.st_structure) ^ " : "
                          ^ PP.pp_tpj_compact env delta pot zc ^ "\n")
    else ()
  end
  ; check_exp trace env delta pot p zc ext mode prmode


 (* judgmental constructs: id, cut, spawn, call *)
and check_exp trace env delta pot exp zc ext mode prmode = match (exp.A.st_structure) with
    A.Fwd(x,y) ->
      begin
        let {A.shared = sdelta ; A.linear = ldelta ; A.ordered = _odelta} = delta in
        let tx = chan_of zc in
        let () =
          if not (eq_name x tx)
          then E.error_unknown_var_right (x) (exp.A.st_data)
          else if not (eq_mode x tx)
          then E.error_mode_mismatch (x, tx) (exp.A.st_data)
          else if not (eq_mode x y)
          then error (exp.A.st_data) ("mode mismatch: " ^ PP.pp_chan x ^ " != " ^ PP.pp_chan y)
          else ()
        in
        let c = tp_of zc in
        if A.is_shared env c
        then
          begin
            if List.length sdelta = 0
            then error (exp.A.st_data) ("shared context empty while offered channel is shared")
            else if not (checktp y sdelta)
            then E.error_unknown_var_ctx (y) (exp.A.st_data)
            else
              let a = find_stp y delta (exp.A.st_data) in
              (* "subtype is sufficient with subsync types" *)
              if subtp env a c
              then staug (A.Fwd(x,y)) exp.A.st_data
              else error (exp.A.st_data) ("left type " ^ PP.pp_tp_compact env a ^ " not subtype of right type " ^
              PP.pp_tp_compact env c)
          end
        else
          begin
            let (ty, a) = List.hd ldelta in
            if List.length ldelta <> 1
            then error (exp.A.st_data) ("linear context " ^ PP.pp_lsctx env ldelta ^ " must have only one channel")
            else if not (eq_name y ty)
            then E.error_unknown_var_ctx (y) (exp.A.st_data)
            else if not (eq_mode y ty)
            then E.error_mode_mismatch (y, ty) (exp.A.st_data)
            else if not (eq pot zero)
            then error (exp.A.st_data) ("unconsumed potential: " ^ pp_uneq pot zero)
            (* "subtype is sufficient with subsync types" *)
            else if subtp env a c
            then staug (A.Fwd(x,y)) exp.A.st_data
            else error (exp.A.st_data) ("left type " ^ PP.pp_tp_compact env a ^ " not equal to right type " ^
                       PP.pp_tp_compact env c)
          end
      end
  | A.Spawn(x,f,xs,q) ->
      begin
        match A.lookup_expdec env f with
            None -> E.error_undeclared (f) (exp.A.st_data)
          | Some (ctx,lpot,(x',a'),mdef) ->
              let (_s,_x,mx) = x in
              if check_tp x delta || checktp x [zc]
              then error (exp.A.st_data) ("variable " ^ name_of x ^ " is not fresh")
              else if not (ge pot lpot)
              then error (exp.A.st_data) ("insufficient potential to spawn: " ^ pp_lt pot lpot)
              else if not (eq_mode x x')
              then E.error_mode_mismatch (x, x') (exp.A.st_data)
              else if not (eqmode mx mdef)
              then error (exp.A.st_data) ("mode mismatch: expected " ^ PP.pp_mode mdef ^ " at declaration, found: " ^ PP.pp_chan x)
              else if not (mode_spawn mdef mode)
              then error (exp.A.st_data) ("cannot spawn at mode " ^ PP.pp_mode mdef ^ " when current mode is " ^ PP.pp_mode mode)
              else
                let ctx = join ctx in
                let delta' = match_ctx env ctx xs delta (List.length ctx) (List.length xs) (exp.A.st_data) in
                let q' = check_exp' trace env (add_chan env (x,a') delta') (minus pot lpot) q zc ext mode prmode in
                staug (A.Spawn(x,f,xs,q')) exp.A.st_data
      end
  | A.ExpName(x,f,xs) ->
      begin
        match A.lookup_expdec env f with
          None -> E.error_undeclared (f) (exp.A.st_data)
        | Some (ctx,lpot,(x',a'),mdef) ->
            let (_s,_x,mx) = x in
            if not (eq pot lpot)
            then error (exp.A.st_data) ("potential mismatch for tail call: " ^ pp_uneq pot lpot)
            else if not (eq_mode x x')
            then E.error_mode_mismatch (x, x') (exp.A.st_data)
            else if not (eqmode mx mdef)
            then error (exp.A.st_data) ("mode mismatch: expected " ^ PP.pp_mode mdef ^ " at declaration, found: " ^ PP.pp_chan x)
            else if not (mode_spawn mdef mode)
            then error (exp.A.st_data) ("cannot tail call at mode " ^ PP.pp_mode mdef ^ " when current mode is " ^ PP.pp_mode mode)
            else
              let (z,c) = zc in
              if not (eq_name x z)
              then E.error_unknown_var_right (x) (exp.A.st_data)
              else if not (eq_mode x z)
              then E.error_mode_mismatch (x, z) (exp.A.st_data)
              (* "subtype is sufficient with subsync types" *)
              else if not (subtp env a' c)
              then error (exp.A.st_data) ("type mismatch on right, expected: " ^ PP.pp_tp_compact env a' ^
                              ", found: " ^ PP.pp_tp_compact env c)
              else
                let ctx = join ctx in
                let delta' = match_ctx env ctx xs delta (List.length ctx) (List.length xs) (exp.A.st_data) in
                if List.length delta'.linear <> 0
                then error (exp.A.st_data) ("unconsumed channel(s) from linear context: " ^ PP.pp_lsctx env delta'.linear)
                else staug (A.ExpName(x,f,xs)) exp.A.st_data
      end
  | A.Lab(x,k,p) ->
      begin
        if not (check_ltp x delta)
        then
          if not (checktp x [zc])
          then E.error_unknown_var (x) (exp.A.st_data)
          else (* the type c of z must be internal choice *)
            let (z,c) = zc in
            if not (eq_mode x z)
            then E.error_mode_mismatch (x, z) (exp.A.st_data)
            else if not (mode_lin x)
            then E.error_mode_shared_comm (x) (exp.A.st_data)
            else
            match c with
                A.TpName(v) -> check_exp' trace env delta pot exp (z,A.expd_tp env v) ext mode prmode
              | A.Plus(choices) ->
                  begin
                    match A.lookup_choice choices k with
                        None -> E.error_label_invalid env (k,c,z) (exp.A.st_data)
                      | Some ck ->
                          let p' = check_exp' trace env delta pot p (z,ck) ext mode prmode in
                          staug (A.Lab(x,k,p')) exp.A.st_data
                  end
              | A.PPlus _ | A.PWith _
              | A.With _ | A.One
              | A.Tensor _ | A.Lolli _
              | A.PayPot _ | A.GetPot _
              | A.Up _ | A.Down _
              | A.FArrow _ | A.FProduct _  ->
                  error (exp.A.st_data) ("invalid type of " ^ PP.pp_chan z ^
                                         ", expected internal choice, found: " ^ PP.pp_tp_compact env c)
        else (* the type a of x must be external choice *)
          let a = find_ltp x delta (exp.A.st_data) in
          match a with
              A.TpName(v) -> check_exp' trace env (update_tp env x (A.expd_tp env v) delta) pot exp zc ext mode prmode
            | A.With(choices) ->
                begin
                  match A.lookup_choice choices k with
                      None -> E.error_label_invalid env (k,a,x) (exp.A.st_data)
                    | Some ak ->
                        let p' = check_exp' trace env (update_tp env x ak delta) pot p zc ext mode prmode in
                        staug (A.Lab(x,k,p')) exp.A.st_data
                end
            | A.Plus _ | A.One
            | A.PPlus _ | A.PWith _
            | A.Tensor _ | A.Lolli _
            | A.PayPot _ | A.GetPot _
            | A.Up _ | A.Down _
            | A.FArrow _ | A.FProduct _ ->
              error (exp.A.st_data) ("invalid type of " ^ PP.pp_chan x ^
                         ", expected external choice, found: " ^ PP.pp_tp_compact env a)
      end
  | A.Case(x,branches) ->
      begin
        if not (check_ltp x delta)
        then
          if not (checktp x [zc])
          then E.error_unknown_var (x) (exp.A.st_data)
          else (* the type c of z must be external choice *)
            let (z,c) = zc in
            if not (eq_mode x z)
            then E.error_mode_mismatch (x, z) (exp.A.st_data)
            else if not (mode_lin x)
            then E.error_mode_shared_comm (x) (exp.A.st_data)
            else
            match c with
                A.TpName(v) -> check_exp' trace env delta pot exp (z,A.expd_tp env v) ext mode prmode
              | A.With(choices) -> staug (A.Case(x,check_branchesR trace env delta pot branches z choices (exp.A.st_data) mode prmode)) exp.A.st_data
              | A.Plus _ | A.One
              | A.PPlus _ | A.PWith _
              | A.Tensor _ | A.Lolli _
              | A.PayPot _ | A.GetPot _
              | A.Up _ | A.Down _
              | A.FArrow _ | A.FProduct _ ->
                error (exp.A.st_data) ("invalid type of " ^ PP.pp_chan z ^
                           ", expected external choice, found: " ^ PP.pp_tp_compact env c)
        else (* the type a of x must be internal choice *)
          let a = find_ltp x delta (exp.A.st_data) in
          match a with
              A.TpName(v) -> check_exp' trace env (update_tp env x (A.expd_tp env v) delta) pot exp zc ext mode prmode
            | A.Plus(choices) -> staug (A.Case(x,check_branchesL trace env delta x choices pot branches zc (exp.A.st_data) mode prmode)) exp.A.st_data
            | A.With _ | A.One
            | A.PPlus _ | A.PWith _
            | A.Tensor _ | A.Lolli _
            | A.PayPot _ | A.GetPot _
            | A.Up _ | A.Down _
            | A.FArrow _ | A.FProduct _ ->
                error (exp.A.st_data) ("invalid type of " ^ PP.pp_chan x ^
                                       ", expected internal choice, found: " ^ PP.pp_tp_compact env a)
      end
  | A.PLab(x,k,p) ->
      begin
        if not (check_ltp x delta)
        then
          if not (checktp x [zc])
          then E.error_unknown_var (x) (exp.A.st_data)
          else (* the type c of z must be prob. internal choice *)
            let (z,c) = zc in
            if not (eq_mode x z)
            then E.error_mode_mismatch (x, z) (exp.A.st_data)
            else if not (mode_lin x)
            then E.error_mode_shared_comm (x) (exp.A.st_data)
            else
            match c with
                A.TpName(v) -> check_exp' trace env delta pot exp (z,A.expd_tp env v) ext mode prmode
              | A.PPlus(pchoices) ->
                  begin
                    let choices = List.map (fun (l,_pot,a) -> (l,a)) pchoices in
                    let () = if prob prmode then plabprob k pchoices (exp.A.st_data) in
                    match A.lookup_choice choices k with
                        None -> E.error_label_invalid env (k,c,z) (exp.A.st_data)
                      | Some ck ->
                          let p' = check_exp' trace env delta pot p (z,ck) ext mode prmode in
                          staug (A.PLab(x,k,p')) exp.A.st_data
                  end
              | A.PWith _ | A.One
              | A.Plus _ | A.With _
              | A.Tensor _ | A.Lolli _
              | A.PayPot _ | A.GetPot _
              | A.Up _ | A.Down _
              | A.FArrow _ | A.FProduct _  ->
                  error (exp.A.st_data) ("invalid type of " ^ PP.pp_chan z ^
                                         ", expected prob. internal choice, found: " ^ PP.pp_tp_compact env c)
        else (* the type a of x must be prob. external choice *)
          let a = find_ltp x delta (exp.A.st_data) in
          match a with
              A.TpName(v) -> check_exp' trace env (update_tp env x (A.expd_tp env v) delta) pot exp zc ext mode prmode
            | A.PWith(pchoices) ->
                begin
                  let choices = List.map (fun (l,_pot,a) -> (l,a)) pchoices in
                  let () = if prob prmode then plabprob k pchoices (exp.A.st_data) in
                  match A.lookup_choice choices k with
                      None -> E.error_label_invalid env (k,a,x) (exp.A.st_data)
                    | Some ak ->
                        let p' = check_exp' trace env (update_tp env x ak delta) pot p zc ext mode prmode in
                        staug (A.PLab(x,k,p')) exp.A.st_data
                end
            | A.PPlus _ | A.One
            | A.Plus _ | A.With _
            | A.Tensor _ | A.Lolli _
            | A.PayPot _ | A.GetPot _
            | A.Up _ | A.Down _
            | A.FArrow _ | A.FProduct _ ->
                error (exp.A.st_data) ("invalid type of " ^ PP.pp_chan x ^
                                       ", expected prob. external choice, found: " ^ PP.pp_tp_compact env a)
      end
  | A.PCase(x,pbranches) ->
      begin
        if not (check_ltp x delta)
        then
          if not (checktp x [zc])
          then E.error_unknown_var (x) (exp.A.st_data)
          else (* the type c of z must be prob. external choice *)
            let (z,c) = zc in
            if not (eq_mode x z)
            then E.error_mode_mismatch (x, z) (exp.A.st_data)
            else if not (mode_lin x)
            then E.error_mode_shared_comm (x) (exp.A.st_data)
            else
            match c with
                A.TpName(v) -> check_exp' trace env delta pot exp (z,A.expd_tp env v) ext mode prmode
              | A.PWith(pchoices) ->
                  let (deltal, potl, pbranches') = check_pbranchesR trace env delta pbranches z pchoices (exp.A.st_data) mode prmode in
                  let () = match_probs_ctx env delta deltal (exp.A.st_data) in
                  if not (eq pot potl)
                  then error (exp.A.st_data) ("potential mismatch in pcase: " ^ pp_uneq pot potl)
                  else staug (A.PCase(x,pbranches')) exp.A.st_data
              | A.PPlus _ | A.One
              | A.Plus _ | A.With _
              | A.Tensor _ | A.Lolli _
              | A.PayPot _ | A.GetPot _
              | A.Up _ | A.Down _
              | A.FArrow _ | A.FProduct _ ->
                  error (exp.A.st_data) ("invalid type of " ^ PP.pp_chan z ^
                                         ", expected prob. external choice, found: " ^ PP.pp_tp_compact env c)
        else (* the type a of x must be prob. internal choice *)
          let a = find_ltp x delta (exp.A.st_data) in
          match a with
              A.TpName(v) -> check_exp' trace env (update_tp env x (A.expd_tp env v) delta) pot exp zc ext mode prmode
            | A.PPlus(pchoices) ->
                let (z,c) = zc in
                let (deltal, potl, cl, pbranches') = check_pbranchesL trace env delta x pchoices pbranches zc (exp.A.st_data) mode prmode in
                let () = match_probs_ctx env delta deltal (exp.A.st_data) in
                let () = match_probs env z c cl (exp.A.st_data) in
                if not (eq pot potl)
                then error (exp.A.st_data) ("potential mismatch in pcase: " ^ pp_uneq pot potl)
                else staug (A.PCase(x,pbranches')) exp.A.st_data
            | A.PWith _ | A.One
            | A.Plus _ | A.With _
            | A.Tensor _ | A.Lolli _
            | A.PayPot _ | A.GetPot _
            | A.Up _ | A.Down _
            | A.FArrow _ | A.FProduct _ ->
                error (exp.A.st_data) ("invalid type of " ^ PP.pp_chan x ^
                                       ", expected prob. internal choice, found: " ^ PP.pp_tp_compact env a)
      end
  | A.Flip(A.Arith pr,p1,p2) ->
      begin
        let deltaHH = if prob prmode then gen_prob_ctx env delta p1 None else delta in
        let deltaTT = if prob prmode then gen_prob_ctx env delta p2 None else delta in
        let (z,c) = zc in
        let cHH = if prob prmode then gen_prob_tpR env p1 z c else c in
        let cTT = if prob prmode then gen_prob_tpR env p2 z c else c in
        let potHH = I.fresh_potvar () in
        let potTT = I.fresh_potvar () in
        let prpotHH = R.Mult(pr, potHH) in
        let prpotTT = R.Mult(comp pr, potTT) in
        let potHHTT = R.Add(prpotHH, prpotTT) in
        let p1' = check_exp' trace env deltaHH (A.Arith potHH) p1 (z,cHH) (p1.A.st_data) mode prmode in
        let p2' = check_exp' trace env deltaTT (A.Arith potTT) p2 (z,cTT) (p2.A.st_data) mode prmode in
        let () = match_probs_ctx env delta [("HH", A.Arith pr, deltaHH); ("TT", A.Arith (comp pr), deltaTT)] (exp.A.st_data) in
        let () = match_probs env z c [("HH", A.Arith pr, cHH); ("TT", A.Arith (comp pr), cTT)] (exp.A.st_data) in
        if not (eq pot (A.Arith potHHTT))
        then error (exp.A.st_data) ("potential mismatch in flip: " ^ pp_uneq pot (A.Arith potHHTT))
        else staug (A.Flip(A.Arith pr,p1',p2')) exp.A.st_data
      end
  | A.Flip(A.Star,_p1,_p2) -> raise UnknownTypeError
  | A.Send(x,w,p) ->
      begin
        if not (check_tp w delta)
        then E.error_unknown_var_ctx (w) (exp.A.st_data)
        else if check_ltp w delta
        then (* type of w is linear *)
          begin
            let a' = find_ltp w delta (exp.A.st_data) in
            if not (check_tp x delta)
            then
              if not (checktp x [zc])
              then E.error_unknown_var (x) (exp.A.st_data)
              else (* the type c of z must be tensor *)
                let (z,c) = zc in
                if not (eq_mode x z)
                then E.error_mode_mismatch (x, z) (exp.A.st_data)
                else if not (mode_lin x)
                then E.error_mode_shared_comm (x) (exp.A.st_data)
                else
                match c with
                    A.TpName(v) -> check_exp' trace env delta pot exp (z,A.expd_tp env v) ext mode prmode
                  | A.Tensor(a,b,m) ->
                      let (_sw,_w,mw) = w in
                      if not (eqmode m mw)
                      then error (exp.A.st_data) ("mode mismatch, expected at tensor: " ^ PP.pp_mode m ^ ", found: " ^ PP.pp_chan w)
                      (* subtype is sufficient with subsync types *)
                      else if not (subtp env a' a)
                      then error (exp.A.st_data)
                                 ("type mismatch: type of " ^ PP.pp_chan w ^
                                  ", expected: " ^ PP.pp_tp_compact env a ^
                                  ", found: " ^ PP.pp_tp_compact env a')
                      else
                        let p' = check_exp' trace env (remove_tp w delta) pot p (z,b) ext mode prmode in
                        staug (A.Send(x,w,p')) exp.A.st_data
                  | A.Plus _ | A.With _
                  | A.PPlus _ | A.PWith _
                  | A.One | A.Lolli _
                  | A.PayPot _ | A.GetPot _
                  | A.Up _ | A.Down _
                  | A.FArrow _ | A.FProduct _ ->
                      error (exp.A.st_data) ("invalid type of " ^ PP.pp_chan x ^
                                             ", expected tensor, found: " ^ PP.pp_tp_compact env c)
            else (* the type a of x must be lolli *)
              let d = find_ltp x delta (exp.A.st_data) in
              match d with
                  A.TpName(v) -> check_exp' trace env (update_tp env x (A.expd_tp env v) delta) pot exp zc ext mode prmode
                | A.Lolli(a,b,m) ->
                    let (_sw,_w,mw) = w in
                    if not (eqmode m mw)
                    then error (exp.A.st_data) ("mode mismatch, expected at lolli: " ^ PP.pp_mode m ^ ", found: " ^ PP.pp_chan w)
                    (* "subtype is sufficient with subsync types" *)
                    else if not (subtp env a' a)
                    then error (exp.A.st_data) ("type mismatch: type of " ^ PP.pp_chan w ^
                                    ", expected: " ^ PP.pp_tp_compact env a ^
                                    ", found: " ^ PP.pp_tp_compact env a')
                    else
                      let p' = check_exp' trace env (update_tp env x b (remove_tp w delta)) pot p zc ext mode prmode in
                      staug (A.Send(x,w,p')) exp.A.st_data
                | A.Plus _ | A.With _
                | A.PPlus _ | A.PWith _
                | A.One | A.Tensor _
                | A.PayPot _ | A.GetPot _
                | A.Up _ | A.Down _
                | A.FArrow _ | A.FProduct _ ->
                    error (exp.A.st_data) ("invalid type of " ^ PP.pp_chan x ^
                                           ", expected lolli, found: " ^ PP.pp_tp_compact env d)
          end
        else (* the type of w is shared *)
          begin
            let a' = find_stp w delta (exp.A.st_data) in
            if not (check_tp x delta)
            then
              if not (checktp x [zc])
              then E.error_unknown_var (x) (exp.A.st_data)
              else (* the type c of z must be tensor *)
                let (z,c) = zc in
                if not (eq_mode x z)
                then E.error_mode_mismatch (x, z) (exp.A.st_data)
                else if not (mode_lin x)
                then E.error_mode_shared_comm (x) (exp.A.st_data)
                else
                match c with
                    A.TpName(v) -> check_exp' trace env delta pot exp (z,A.expd_tp env v) ext mode prmode
                  | A.Tensor(a,b,m) ->
                      let (_sw,_w,mw) = w in
                      if not (eqmode m mw)
                      then error (exp.A.st_data) ("mode mismatch, expected at tensor: " ^ PP.pp_mode m ^ ", found: " ^ PP.pp_chan w)
                      (* "subtype is sufficient with subsync types" *)
                      else if not (subtp env a' a)
                      then error (exp.A.st_data) ("type mismatch: type of " ^ PP.pp_chan w ^
                                      ", expected: " ^ PP.pp_tp_compact env a ^
                                      ", found: " ^ PP.pp_tp_compact env a')
                      else
                        let p' = check_exp' trace env delta pot p (z,b) ext mode prmode in
                        staug (A.Send(x,w,p')) exp.A.st_data
                  | A.Plus _ | A.With _
                  | A.PPlus _ | A.PWith _
                  | A.One | A.Lolli _
                  | A.PayPot _ | A.GetPot _
                  | A.Up _ | A.Down _
                  | A.FArrow _ | A.FProduct _ ->
                    error (exp.A.st_data) ("invalid type of " ^ PP.pp_chan x ^
                               ", expected tensor, found: " ^ PP.pp_tp_compact env c)
            else (* the type a of x must be lolli *)
              let d = find_ltp x delta (exp.A.st_data) in
              match d with
                  A.TpName(v) -> check_exp' trace env (update_tp env x (A.expd_tp env v) delta) pot exp zc ext mode prmode
                | A.Lolli(a,b,m) ->
                    let (_sw,_w,mw) = w in
                    if not (eqmode m mw)
                    then error (exp.A.st_data) ("mode mismatch, expected at lolli: " ^ PP.pp_mode m ^ ", found: " ^ PP.pp_chan w)
                    (* "subtype is sufficient with subsync types" *)
                    else if not (subtp env a' a)
                    then error (exp.A.st_data) ("type mismatch: type of " ^ PP.pp_chan w ^
                                    ", expected: " ^ PP.pp_tp_compact env a ^
                                    ", found: " ^ PP.pp_tp_compact env a')
                    else
                      let p' = check_exp' trace env (update_tp env x b delta) pot p zc ext mode prmode in
                      staug (A.Send(x,w,p')) exp.A.st_data
                | A.Plus _ | A.With _
                | A.PPlus _ | A.PWith _
                | A.One | A.Tensor _
                | A.PayPot _ | A.GetPot _
                | A.Up _ | A.Down _
                | A.FArrow _ | A.FProduct _ ->
                  error (exp.A.st_data) ("invalid type of " ^ PP.pp_chan x ^
                             ", expected lolli, found: " ^ PP.pp_tp_compact env d)
          end
      end
  | A.Recv(x,y,p) ->
      begin
        if check_tp y delta || checktp y [zc]
        then error (exp.A.st_data) ("variable " ^ name_of y ^ " is not fresh")
        else
          if not (check_ltp x delta)
          then
            if not (checktp x [zc])
            then E.error_unknown_var (x) (exp.A.st_data)
            else (* the type c of z must be lolli *)
              let (z,c) = zc in
              if not (eq_mode x z)
              then E.error_mode_mismatch (x, z) (exp.A.st_data)
              else if not (mode_lin x)
              then E.error_mode_shared_comm (x) (exp.A.st_data)
              else
              match c with
                  A.TpName(v) -> check_exp' trace env delta pot exp (z,A.expd_tp env v) ext mode prmode
                | A.Lolli(a,b,m) ->
                    let (_sy,_y,my) = y in
                    if not (eqmode m my)
                    then error (exp.A.st_data) ("mode mismatch, expected at lolli: " ^ PP.pp_mode m ^ ", found: " ^ PP.pp_chan y)
                    else if not (mode_recv m mode)
                    then error (exp.A.st_data) ("cannot receive at mode " ^ PP.pp_mode m ^ " when current mode is " ^ PP.pp_mode mode)
                    else
                      let p' = check_exp' trace env (add_chan env (y,a) delta) pot p (z,b) ext mode prmode in
                      staug (A.Recv(x,y,p')) exp.A.st_data
                | A.Plus _ | A.With _
                | A.PPlus _ | A.PWith _
                | A.One | A.Tensor _
                | A.PayPot _ | A.GetPot _
                | A.Up _ | A.Down _
                | A.FArrow _ | A.FProduct _ ->
                  error (exp.A.st_data) ("invalid type of " ^ PP.pp_chan x ^
                             ", expected lolli, found: " ^ PP.pp_tp_compact env c)
          else (* the type a of x must be tensor *)
            let d = find_ltp x delta (exp.A.st_data) in
            match d with
                A.TpName(v) -> check_exp' trace env (update_tp env x (A.expd_tp env v) delta) pot exp zc ext mode prmode
              | A.Tensor(a,b,m) ->
                  let (_sy,_y,my) = y in
                  if not (eqmode m my)
                  then error (exp.A.st_data) ("mode mismatch, expected at tensor: " ^ PP.pp_mode m ^ ", found: " ^ PP.pp_chan y)
                  else if not (mode_recv m mode)
                  then error (exp.A.st_data) ("cannot receive at mode " ^ PP.pp_mode m ^ " when current mode is " ^ PP.pp_mode mode)
                  else
                    let p' = check_exp' trace env (add_chan env (y,a) (update_tp env x b delta)) pot p zc ext mode prmode in
                    staug (A.Recv(x,y,p')) exp.A.st_data
              | A.Plus _ | A.With _
              | A.PPlus _ | A.PWith _
              | A.One | A.Lolli _
              | A.PayPot _ | A.GetPot _
              | A.Up _ | A.Down _
              | A.FArrow _ | A.FProduct _ ->
                error (exp.A.st_data) ("invalid type of " ^ PP.pp_chan x ^
                           ", expected tensor, found: " ^ PP.pp_tp_compact env d)
      end
  | A.Close(x) ->
      begin
        let {A.shared = _sdelta ; A.linear = ldelta ; A.ordered = _odelta} = delta in
        if List.length ldelta > 0
        then error (exp.A.st_data) ("linear context " ^ PP.pp_lsctx env ldelta ^ " not empty")
        else if not (checktp x [zc])
        then E.error_unknown_var (x) (exp.A.st_data)
        else if not (eq pot zero)
        then error (exp.A.st_data) ("unconsumed potential: " ^ pp_uneq pot zero)
        else
          let (z,c) = zc in
          if not (eq_mode x z)
          then E.error_mode_mismatch (x, z) (exp.A.st_data)
          else if not (mode_lin x)
          then E.error_mode_shared_comm (x) (exp.A.st_data)
          else
          (* "subtype is sufficient with subsync types" *)
          if not (subtp env c A.One)
          then error (exp.A.st_data) ("type mismatch: type of " ^ PP.pp_chan x ^ ", expected: 1, " ^
                          "found: " ^ PP.pp_tp_compact env c)
          else staug (A.Close(x)) exp.A.st_data
      end
  | A.Wait(x,p) ->
      begin
        if not (check_ltp x delta)
        then E.error_unknown_var (x) (exp.A.st_data)
        else
          let a = find_ltp x delta (exp.A.st_data) in
          (* "subtype is sufficient with subsync types" *)
          if not (subtp env a A.One)
          then error (exp.A.st_data) ("type mismatch: type of " ^ PP.pp_chan x ^ ", expected: 1, " ^
                          " found: " ^ PP.pp_tp_compact env a)
          else
            let p' = check_exp' trace env (remove_tp x delta) pot p zc ext mode prmode in
            staug (A.Wait(x,p')) exp.A.st_data
      end
  | A.Work(pot',p) ->
      begin
        if not (ge pot pot')
        then error (exp.A.st_data) ("insufficient potential to work: " ^ pp_lt pot pot')
        else if not (ge pot' zero)
        then error (exp.A.st_data) ("potential not positive: " ^ pp_lt pot' zero)
        else
          let p' = check_exp' trace env delta (minus pot pot') p zc ext mode prmode in
          staug (A.Work(pot',p')) exp.A.st_data
      end
  | A.Pay(x,epot,p) ->
      begin
        if not (check_ltp x delta)
        then
          if not (checktp x [zc])
          then E.error_unknown_var (x) (exp.A.st_data)
          else (* the type c of z must be paypot *)
            let (z,c) = zc in
            if not (eq_mode x z)
            then E.error_mode_mismatch (x, z) (exp.A.st_data)
            else if not (mode_lin x)
            then E.error_mode_shared_comm (x) (exp.A.st_data)
            else
            match c with
                A.TpName(v) -> check_exp' trace env delta pot exp (z,A.expd_tp env v) ext mode prmode
              | A.PayPot(tpot,c') ->
                  if not (eq epot tpot)
                  then error (exp.A.st_data) ("potential mismatch: potential in type does not match " ^
                                  "potential in expression: " ^ pp_uneq epot tpot)
                  else if not (ge pot tpot)
                  then error (exp.A.st_data) ("insufficient potential to pay: " ^ pp_lt pot tpot)
                  else
                    let p' = check_exp' trace env delta (minus pot tpot) p (z,c') ext mode prmode in
                    staug (A.Pay(x,epot,p')) exp.A.st_data
              | A.Plus _ | A.With _
              | A.PPlus _ | A.PWith _
              | A.Tensor _ | A.Lolli _
              | A.One | A.GetPot _
              | A.Up _ | A.Down _
              | A.FArrow _ | A.FProduct _ ->
              error (exp.A.st_data) ("invalid type of " ^ PP.pp_chan x ^
                                                ", expected paypot, found: " ^ PP.pp_tp_compact env c)
        else (* the type a of x must be getpot *)
          let a = find_ltp x delta (exp.A.st_data) in
          match a with
              A.TpName(v) -> check_exp' trace env (update_tp env x (A.expd_tp env v) delta) pot exp zc ext mode prmode
            | A.GetPot(tpot,a') ->
                if not (eq epot tpot)
                then error (exp.A.st_data) ("potential mismatch: potential in type does not match " ^
                                "potential in expression: " ^ pp_uneq epot tpot)
                else if not (ge pot epot)
                then error (exp.A.st_data) ("insufficient potential to pay: " ^ pp_lt pot epot)
                else
                  let p' = check_exp' trace env (update_tp env x a' delta) (minus pot epot) p zc ext mode prmode in
                  staug (A.Pay(x,epot,p')) exp.A.st_data
            | A.Plus _ | A.With _
            | A.PPlus _ | A.PWith _
            | A.Tensor _ | A.Lolli _
            | A.One | A.PayPot _
            | A.Up _ | A.Down _
            | A.FArrow _ | A.FProduct _ ->
              error (exp.A.st_data) ("invalid type of " ^ PP.pp_chan x ^
                                              ", expected getpot, found: " ^ PP.pp_tp_compact env a)
      end
  | A.Get(x,epot,p) ->
      begin
        if not (check_ltp x delta)
        then
          if not (checktp x [zc])
          then E.error_unknown_var (x) (exp.A.st_data)
          else (* the type c of z must be getpot *)
            let (z,c) = zc in
            if not (eq_mode x z)
            then E.error_mode_mismatch (x, z) (exp.A.st_data)
            else if not (mode_lin x)
            then E.error_mode_shared_comm (x) (exp.A.st_data)
            else
            match c with
                A.TpName(v) -> check_exp' trace env delta pot exp (z,A.expd_tp env v) ext mode prmode
              | A.GetPot(tpot,c') ->
                  if not (eq epot tpot)
                  then error (exp.A.st_data) ("potential mismatch: potential in type does not match " ^
                                  "potential in expression: " ^ pp_uneq epot tpot)
                  else
                    let p' = check_exp' trace env delta (plus pot epot) p (z,c') ext mode prmode in
                    staug (A.Get(x,epot,p')) exp.A.st_data
              | A.Plus _ | A.With _
              | A.PPlus _ | A.PWith _
              | A.Tensor _ | A.Lolli _
              | A.One | A.PayPot _
              | A.Up _ | A.Down _
              | A.FArrow _ | A.FProduct _ -> error (exp.A.st_data) ("invalid type of " ^ PP.pp_chan x ^
                                                ", expected getpot, found: " ^ PP.pp_tp_compact env c)
        else (* the type a of x must be paypot *)
          let a = find_ltp x delta (exp.A.st_data) in
          match a with
              A.TpName(v) -> check_exp' trace env (update_tp env x (A.expd_tp env v) delta) pot exp zc ext mode prmode
            | A.PayPot(tpot,a') ->
                if not (eq epot tpot)
                then error (exp.A.st_data) ("potential mismatch: potential in type does not match " ^
                                "potential in expression: " ^ pp_uneq epot tpot)
                else
                  let p' = check_exp' trace env (update_tp env x a' delta) (plus pot epot) p zc ext mode prmode in
                  staug (A.Get(x,epot,p')) exp.A.st_data
            | A.Plus _ | A.With _
            | A.PPlus _ | A.PWith _
            | A.Tensor _ | A.Lolli _
            | A.One | A.GetPot _
            | A.Up _ | A.Down _
            | A.FArrow _ | A.FProduct _ -> error (exp.A.st_data) ("invalid type of " ^ PP.pp_chan x ^
                                              ", expected paypot, found: " ^ PP.pp_tp_compact env a)
      end
  | A.Acquire(x,y,p) ->
      begin
        if check_tp y delta || checktp y [zc]
        then error (exp.A.st_data) ("variable " ^ name_of y ^ " is not fresh")
        else if not (check_stp x delta)
        then E.error_unknown_var_ctx (x) (exp.A.st_data)
        else if not (mode_L y)
        then error (exp.A.st_data) ("mode mismatch of acquired channel: expected L, found " ^ PP.pp_chan y)
        else if not (mode_S x)
        then error (exp.A.st_data) ("mode mismatch of acquiring channel: expected S, found " ^ PP.pp_chan x)
        else
          let a = find_stp x delta (exp.A.st_data) in
          match a with
              A.TpName(v) -> check_exp' trace env (update_tp env x (A.expd_tp env v) delta) pot exp zc ext mode prmode
            | A.Up(a') ->
                let p' = check_exp' trace env (add_chan env (y,a') (remove_tp x delta)) pot p zc ext mode prmode in
                staug (A.Acquire(x,y,p')) exp.A.st_data
            | A.Plus _ | A.With _
            | A.PPlus _ | A.PWith _
            | A.Tensor _ | A.Lolli _
            | A.One
            | A.PayPot _ | A.GetPot _
            | A.Down _
            | A.FArrow _ | A.FProduct _ -> error (exp.A.st_data) ("invalid type of " ^ PP.pp_chan x ^
                                     ", expected up, found: " ^ PP.pp_tp_compact env a)
      end
  | A.Accept(x,y,p) ->
      begin
        if check_tp y delta || checktp y [zc]
        then error (exp.A.st_data) ("variable " ^ name_of y ^ " is not fresh")
        else if not (checktp x [zc])
        then E.error_unknown_var_right (x) (exp.A.st_data)
        else if not (mode_L y)
        then error (exp.A.st_data) ("mode mismatch of accepted channel: expected L, found " ^ PP.pp_chan y)
        else if not (mode_S x)
        then error (exp.A.st_data) ("mode mismatch of accepting channel: expected S, found " ^ PP.pp_chan x)
        else if not (purelin delta)
        then error (exp.A.st_data) ("independence principle violated: " ^
                        "expected pure linear context, found: " ^
                        PP.pp_lsctx env delta.linear)
        else
          let (z,c) = zc in
          if not (eq_mode x z)
          then E.error_mode_mismatch (x, z) (exp.A.st_data)
          else
          match c with
              A.TpName(v) -> check_exp' trace env delta pot exp (z,A.expd_tp env v) ext mode prmode
            | A.Up(c') ->
                let p' = check_exp' trace env delta pot p (y,c') ext A.Linear prmode in
                staug (A.Accept(x,y,p')) exp.A.st_data
            | A.Plus _ | A.With _
            | A.PPlus _ | A.PWith _
            | A.Tensor _ | A.Lolli _
            | A.One
            | A.PayPot _ | A.GetPot _
            | A.Down _
            | A.FArrow _ | A.FProduct _ -> error (exp.A.st_data) ("invalid type of " ^ PP.pp_chan x ^
                                     ", expected up, found: " ^ PP.pp_tp_compact env c)
      end
  | A.Release(x,y,p) ->
      begin
        if check_tp y delta || checktp y [zc]
        then error (exp.A.st_data) ("variable " ^ name_of y ^ " is not fresh")
        else if not (check_ltp x delta)
        then E.error_unknown_var_ctx (x) (exp.A.st_data)
        else if not (mode_S y)
        then error (exp.A.st_data) ("mode mismatch of released channel: expected S, found " ^ PP.pp_chan y)
        else if not (mode_L x)
        then error (exp.A.st_data) ("mode mismatch of releasing channel: expected L, found " ^ PP.pp_chan x)
        else
          let a = find_ltp x delta (exp.A.st_data) in
          match a with
              A.TpName(v) -> check_exp' trace env (update_tp env x (A.expd_tp env v) delta) pot exp zc ext mode prmode
            | A.Down(a') ->
                let p' = check_exp' trace env (add_chan env (y,a') (remove_tp x delta)) pot p zc ext mode prmode in
                staug (A.Release(x,y,p')) exp.A.st_data
            | A.Plus _ | A.With _
            | A.PPlus _ | A.PWith _
            | A.Tensor _ | A.Lolli _
            | A.One
            | A.PayPot _ | A.GetPot _
            | A.Up _
            | A.FArrow _ | A.FProduct _ -> error (exp.A.st_data) ("invalid type of " ^ PP.pp_chan x ^
                                     ", expected down, found: " ^ PP.pp_tp_compact env a)
      end
  | A.Detach(x,y,p) ->
      begin
        if check_tp y delta || checktp y [zc]
        then error (exp.A.st_data) ("variable " ^ name_of y ^ " is not fresh")
        else if not (checktp x [zc])
        then E.error_unknown_var_right (x) (exp.A.st_data)
        else if not (mode_S y)
        then error (exp.A.st_data) ("mode mismatch of detached channel: expected S, found " ^ PP.pp_chan y)
        else if not (mode_L x)
        then error (exp.A.st_data) ("mode mismatch of detaching channel: expected L, found " ^ PP.pp_chan x)
        else if not (purelin delta)
        then error (exp.A.st_data) ("independence principle violated: " ^
                        "expected empty linear context, found: " ^
                        PP.pp_lsctx env delta.linear)
        else
          let (z,c) = zc in
          if not (eq_mode x z)
          then E.error_mode_mismatch (x, z) (exp.A.st_data)
          else
          match c with
              A.TpName(v) -> check_exp' trace env delta pot exp (z,A.expd_tp env v) ext mode prmode
            | A.Down(c') ->
                let p' = check_exp' trace env delta pot p (y,c') ext A.Shared prmode in
                staug (A.Detach(x,y,p')) exp.A.st_data
            | A.Plus _ | A.With _
            | A.PPlus _ | A.PWith _
            | A.Tensor _ | A.Lolli _
            | A.One
            | A.PayPot _ | A.GetPot _
            | A.Up _
            | A.FArrow _ | A.FProduct _ -> error (exp.A.st_data) ("invalid type of " ^ PP.pp_chan x ^
                                     ", expected down, found: " ^ PP.pp_tp_compact env c)
      end
  | A.SendF(x,e,p) ->
      begin
        if not (check_tp x delta)
        then
          if not (checktp x [zc])
          then E.error_unknown_var (x) (exp.A.st_data)
          else (* the type c of z must be fproduct *)
            let (z,c) = zc in
            if not (eq_mode x z)
            then E.error_mode_mismatch (x, z) (exp.A.st_data)
            else if not (mode_lin x)
            then E.error_mode_shared_comm (x) (exp.A.st_data)
            else
            match c with
                A.TpName(v) -> check_exp' trace env delta pot exp (z,A.expd_tp env v) ext mode prmode
              | A.FProduct(t,b) ->
                  let (delta', pot', e') = check_fexp_simple trace env delta pot e t ext mode true prmode in
                  let p' = check_exp' trace env delta' pot' p (z,b) ext mode prmode in
                  staug (A.SendF(x,e',p')) exp.A.st_data
              | A.Plus _ | A.With _
              | A.PPlus _ | A.PWith _
              | A.One | A.Lolli _ | A.Tensor _
              | A.PayPot _ | A.GetPot _
              | A.Up _ | A.Down _
              | A.FArrow _ ->
                error (exp.A.st_data) ("invalid type of " ^ PP.pp_chan x ^
                            ", expected fproduct, found: " ^ PP.pp_tp_compact env c)
        else (* the type a of x must be farrow *)
          let d = find_ltp x delta (exp.A.st_data) in
          match d with
              A.TpName(v) -> check_exp' trace env (update_tp env x (A.expd_tp env v) delta) pot exp zc ext mode prmode
            | A.FArrow(t,b) ->
                let (delta', pot', e') = check_fexp_simple trace env delta pot e t ext mode true prmode in
                let p' = check_exp' trace env (update_tp env x b delta') pot' p zc ext mode prmode in
                staug (A.SendF(x,e',p')) exp.A.st_data
            | A.Plus _ | A.With _
            | A.PPlus _ | A.PWith _
            | A.One | A.Tensor _ | A.Lolli _
            | A.PayPot _ | A.GetPot _
            | A.Up _ | A.Down _
            | A.FProduct _ ->
              error (exp.A.st_data) ("invalid type of " ^ PP.pp_chan x ^
                          ", expected farrow, found: " ^ PP.pp_tp_compact env d)
      end
  | A.RecvF(x,y,p) ->
      begin
        if check_ftp y delta
        then error (exp.A.st_data) ("variable " ^ y ^ " is not fresh")
        else
          if not (check_ltp x delta)
          then
            if not (checktp x [zc])
            then E.error_unknown_var (x) (exp.A.st_data)
            else (* the type c of z must be farrow *)
              let (z,c) = zc in
              if not (eq_mode x z)
              then E.error_mode_mismatch (x, z) (exp.A.st_data)
              else if not (mode_lin x)
              then E.error_mode_shared_comm (x) (exp.A.st_data)
              else
              match c with
                  A.TpName(v) -> check_exp' trace env delta pot exp (z,A.expd_tp env v) ext mode prmode
                | A.FArrow(t,b) ->
                    let p' = check_exp' trace env (add_var (y,t) delta) pot p (z,b) ext mode prmode in
                    staug (A.RecvF(x,y,p')) exp.A.st_data
                | A.Plus _ | A.With _
                | A.PPlus _ | A.PWith _
                | A.One | A.Tensor _ | A.Lolli _
                | A.PayPot _ | A.GetPot _
                | A.Up _ | A.Down _
                | A.FProduct _ ->
                  error (exp.A.st_data) ("invalid type of " ^ PP.pp_chan x ^
                             ", expected farrow, found: " ^ PP.pp_tp_compact env c)
          else (* the type a of x must be fproduct *)
            let d = find_ltp x delta (exp.A.st_data) in
            match d with
                A.TpName(v) -> check_exp' trace env (update_tp env x (A.expd_tp env v) delta) pot exp zc ext mode prmode
              | A.FProduct(t,b) ->
                  let p' = check_exp' trace env (add_var (y,t) (update_tp env x b delta)) pot p zc ext mode prmode in
                  staug (A.RecvF(x,y,p')) exp.A.st_data
              | A.Plus _ | A.With _
              | A.PPlus _ | A.PWith _
              | A.One | A.Lolli _ | A.Tensor _
              | A.PayPot _ | A.GetPot _
              | A.Up _ | A.Down _
              | A.FArrow _ ->
                error (exp.A.st_data) ("invalid type of " ^ PP.pp_chan x ^
                           ", expected fproduct, found: " ^ PP.pp_tp_compact env d)
      end
  | A.Let(x,e,p) ->
      begin
        let (delta', pot', t, e') = synth_fexp_simple trace env delta pot e ext mode false prmode in
        let p' = check_exp' trace env (add_var (x,t) delta') pot' p zc ext mode prmode in
        staug (A.Let(x,e',p')) exp.A.st_data
      end
  | A.IfS(e,p1,p2) ->
      begin
        let (delta', pot', e') = check_fexp_simple trace env delta pot e A.Boolean ext mode false prmode in
        let p1' = check_exp' trace env delta' pot' p1 zc ext mode prmode in
        let p2' = check_exp' trace env delta' pot' p2 zc ext mode prmode in
        staug (A.IfS(e',p1',p2')) exp.A.st_data
      end
  | A.MakeChan(x,a,t,p) ->
      begin
        if check_tp x delta || checktp x [zc]
        then error (exp.A.st_data) ("variable " ^ name_of x ^ " is not fresh")
        else if not (mode_S x)
        then error (exp.A.st_data) (PP.pp_chan x ^ "not shared; can only create shared channels")
        else
          let p' = check_exp' trace env (add_chan env (x,a) delta) pot p zc ext mode prmode in
          staug (A.MakeChan(x,a,t,p')) exp.A.st_data
      end
  | A.Abort -> staug (A.Abort) exp.A.st_data
  | A.Print(l,args,p) -> 
      begin
        let () = check_printable_list delta (exp.A.st_data) l args (List.length (filter_args l)) (List.length args) in
        let p' = check_exp' trace env delta pot p zc ext mode prmode in
        staug (A.Print(l,args,p')) exp.A.st_data
      end

and check_pbranchesR trace env delta branches z pchoices ext mode prmode = match branches, pchoices with
    (l1,_pr,p)::branchestl, (l2,prc,c)::pchoicestl ->
      begin
        let () = if trace then print_string ("| " ^ l1 ^ " => \n") else () in
        let () = if l1 = l2 then () else E.error_label_mismatch (l1, l2) ext in
        let deltal = if prob prmode then gen_prob_ctx env delta p None else delta in
        let potl = pot_branch prc in
        let prpotl = mult prc potl in
        let p' = check_exp' trace env deltal potl p (z,c) (p.A.st_data) mode prmode in
        let (deltatl, pottl, branchestl') = check_pbranchesR trace env delta branchestl z pchoicestl ext mode prmode in
        ((l1, prc, deltal)::deltatl, add prpotl pottl, (l1,prc,p')::branchestl')
      end
  | [], [] -> ([], A.Arith (R.Float(0.)), [])
  | (l,_pr,_p)::_branches', [] ->
      E.error_label_missing_alt (l) ext
  | [], (l,_prob,_c)::_choices' ->
      E.error_label_missing_branch (l) ext

and check_pbranchesL trace env delta x pchoices branches zc ext mode prmode = match pchoices, branches with
    (l1,prc,a)::pchoicestl, (l2,_pr,p)::branchestl ->
      begin
        let () = if trace then print_string ("| " ^ l1 ^ " => \n") else () in
        let () = if l1 = l2 then () else E.error_label_mismatch (l1, l2) ext in
        let deltal = if prob prmode then gen_prob_ctx env delta p (Some x) else delta in
        let potl = pot_branch prc in
        let prpotl = mult prc potl in
        let (z,c) = zc in
        let cl = if prob prmode then gen_prob_tpR env p z c else c in
        let p' = check_exp' trace env (update_tp env x a deltal) potl p (z,cl) (p.A.st_data) mode prmode in
        let (deltatl, pottl, ctl, branchestl') = check_pbranchesL trace env delta x pchoicestl branchestl zc ext mode prmode in
        ((l1, prc, deltal)::deltatl, add prpotl pottl, (l1, prc, cl)::ctl, (l2,prc,p')::branchestl')
      end
  | [], [] -> ([], A.Arith (R.Float(0.)), [], [])
  | [], (l,_pr,_p)::_branches' ->
      E.error_label_missing_alt (l) ext
  | (l,_prob,_a)::_choices', [] ->
      E.error_label_missing_branch (l) ext

and check_branchesR trace env delta pot branches z choices ext mode prmode = match branches, choices with
    (l1,p)::branchestl, (l2,c)::choicestl ->
      begin
        let () = if trace then print_string ("| " ^ l1 ^ " => \n") in
        let () = if l1 = l2 then () else E.error_label_mismatch (l1, l2) ext in
        let p' = check_exp' trace env delta pot p (z,c) (p.A.st_data) mode prmode in
        let branches' = check_branchesR trace env delta pot branchestl z choicestl ext mode prmode in
        (l1,p')::branches'
      end
  | [], [] -> []
  | (l,_p)::_branches', [] ->
      E.error_label_missing_alt (l) ext
  | [], (l,_c)::_choices' ->
      E.error_label_missing_branch (l) ext

and check_branchesL trace env delta x choices pot branches zc ext mode prmode = match choices, branches with
    (l1,a)::choicestl, (l2,p)::branchestl ->
      begin
        let () = if trace then print_string ("| " ^ l1 ^ " => \n") in
        let () = if l1 = l2 then () else E.error_label_mismatch (l1, l2) ext in
        let p' = check_exp' trace env (update_tp env x a delta) pot p zc (p.A.st_data) mode prmode in
        let branches' = check_branchesL trace env delta x choicestl pot branchestl zc ext mode prmode in
        (l2,p')::branches'
      end
  | [], [] -> []
  | [], (l,_p)::_branches' ->
      E.error_label_missing_alt (l) ext
  | (l,_a)::_choices', [] ->
      E.error_label_missing_branch (l) ext;;

(* external interface *)
let checkexp = check_exp';;

let rec find_tp x sdelta ldelta = match sdelta, ldelta with
    [], [] -> raise UnknownTypeError
  | (y,_t1)::sdelta', (z,_t2)::ldelta' ->
      if eq_name x y
      then y
      else if eq_name x z
      then z
      else find_tp x sdelta' ldelta'
  | (y,_t)::sdelta', [] ->
      if eq_name x y
      then y
      else find_tp x sdelta' []
  | [], (z,_t)::ldelta' ->
      if eq_name x z
      then z
      else find_tp x [] ldelta';;

let rec consistent_mode f sdelta ldelta odelta ext =
  match odelta with
      [] -> ()
    | (A.Functional _)::odelta' -> consistent_mode f sdelta ldelta odelta' ext
    | (A.STyped (x,_t))::odelta' ->
        let y = find_tp x sdelta ldelta in
        if not (eq_mode x y)
        then E.error_mode_mismatch (x, y) ext
        else consistent_mode f sdelta ldelta odelta' ext;;

let rec mode_P_list delta = match delta with
    [] -> true
  | (x,_t)::delta' -> if not (mode_P x) then false else mode_P_list delta';;

let pure env f delta x ext =
  let {A.shared = sdelta ; A.linear = ldelta ; A.ordered = odelta} = delta in
  let () = consistent_mode f sdelta ldelta odelta ext in
  if not (mode_P x)
  then error ext ("asset process " ^ f ^ " has offered channel at mode " ^ PP.pp_chan x)
  else if List.length sdelta > 0
  then error ext ("asset process " ^ f ^ " has non-empty shared context: " ^ PP.pp_lsctx env sdelta)
  else if not (mode_P_list ldelta)
  then error ext ("asset process " ^ f ^ " has non-pure linear context: " ^ PP.pp_lsctx env ldelta)
  else ();;

let rec mode_S_list delta = match delta with
    [] -> true
  | (x,_t)::delta' -> if not (mode_S x) then false else mode_S_list delta';;

let shared env f delta x ext =
  let {A.shared = sdelta ; A.linear = ldelta ; A.ordered = odelta} = delta in
  let () = consistent_mode f sdelta ldelta odelta ext in
  if not (mode_S x)
  then error ext ("shared process " ^ f ^ " has offered channel at mode " ^ PP.pp_chan x)
  else if not (mode_S_list sdelta)
  then error ext ("shared process " ^ f ^ " has non-shared context: " ^ PP.pp_lsctx env sdelta)
  else if not (mode_P_list ldelta)
  then error ext ("shared process " ^ f ^ " has non-pure linear context: " ^ PP.pp_lsctx env ldelta)
  else ();;

let rec mode_lin_list delta = match delta with
    [] -> true
  | (x,_t)::delta' -> if not (mode_lin x) then false else mode_lin_list delta';;

let transaction env f delta x ext =
  let {A.shared = sdelta ; A.linear = ldelta ; A.ordered = odelta} = delta in
  let () = consistent_mode f sdelta ldelta odelta ext in
  if not (mode_T x)
  then error ext ("transaction process " ^ f ^ " has offered channel at mode " ^ PP.pp_chan x)
  else if not (mode_S_list sdelta)
  then error ext ("transaction process " ^ f ^ " has shared context not at shared mode: " ^ PP.pp_lsctx env sdelta)
  else if not (mode_lin_list ldelta)
  then error ext ("transaction process " ^ f ^ " has linear context not at linear mode: " ^ PP.pp_lsctx env ldelta)
  else ();;

let rec sum_prob tp = match tp with
    A.Plus(choices) | A.With(choices) -> sum_prob_choices choices
  | A.PPlus(pchoices) | A.PWith(pchoices) ->
      let sum = sum_prob_pchoices pchoices in
      let _b = eq sum pr_one in
      ()
  | A.Tensor(a,b,_m) | A.Lolli(a,b,_m) ->
      let () = sum_prob a in
      let () = sum_prob b in
      ()
  | A.One | A.TpName _ -> ()
  | A.PayPot(_,a) | A.GetPot(_,a)
  | A.Up(a) | A.Down(a)
  | A.FArrow(_,a) | A.FProduct(_,a) -> sum_prob a

and sum_prob_choices cs = match cs with
    [] -> ()
  | (_l,a)::cs' ->  
      let () = sum_prob a in
      sum_prob_choices cs'

and sum_prob_pchoices pcs = match pcs with
    [] -> pr_zero
  | (_l,pr,a)::pcs' ->
      let () = sum_prob a in
      let prtl = sum_prob_pchoices pcs' in
      add pr prtl;;