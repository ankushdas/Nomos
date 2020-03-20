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
    | A.Tensor(_a,b,_m) -> esync env seen b c ext is_shared
    | A.Lolli(_a,b,_m) -> esync env seen b c ext is_shared
     (* Ask Ankush *)
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
          (* Ask Ankush *)
            | A.UndeclaredTp -> error ext ("type " ^ v ^ " undeclared")
        end
    | A.Up(a) -> esync env seen a c ext true
    | A.Down(a) -> esync env seen a c ext false
    | A.FArrow(_t,a) -> esync env seen a c ext is_shared
    | A.FProduct(_t,a) -> esync env seen a c ext is_shared

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

let zero = A.Arith (R.Int 0);;

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
  then print_string ("comparing " ^ PP.pp_tp env a ^ " and " ^ PP.pp_tp env a' ^ "\n")
  else ()
  ; eq_tp env seen a a'

and eq_tp env seen tp tp' = match tp, tp' with
    A.Plus(choice), A.Plus(choice') ->
      eq_choice env seen choice choice'
  | A.With(choice), A.With(choice') ->
      eq_choice env seen choice choice'
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
  eq_str c1 c2 && eq_name c1 c2 && eq_mode c1 c2;;

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

let rec findftp v delta ext = match delta with
    [] -> raise UnknownTypeError
  | (A.Functional (w,t))::delta' ->
      if v = w then t
      else findftp v delta' ext
  | (A.STyped _)::delta' ->
      findftp v delta' ext;;

let find_ftp v delta ext =
  let {A.shared = _sdelta ; A.linear = _ldelta ; A.ordered = odelta} = delta in
  findftp v odelta ext;;

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
              if eqtp env st t
              then match_ctx env sig_ctx' ctx' delta sig_len len ext
              else error ext ("shared type mismatch: type of " ^ PP.pp_chan c ^ " : " ^ PP.pp_tp_compact env t ^
                          " does not match type in declaration: " ^ PP.pp_tp_compact env st)
            end
          else
            begin
              let t = find_ltp c delta ext in
              if eqtp env st t
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
          let t = find_ftp v delta ext in
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
    A.Integer | A.Boolean | A.Address | A.VarT _ -> tp
  | A.ListTP(t,_pot) -> A.ListTP(consume_pot t, A.Arith (R.Int(0)))
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

let rec check_fexp_simple' trace env delta pot (e : A.parsed_expr) tp ext mode isSend =
  begin
    if trace
    then print_string ("Checking: [" ^ PP.pp_mode mode ^ "] : " ^  PP.pp_fexp env 0 (e.A.func_structure) ^ " : "
                          ^ PP.pp_ftp_simple tp ^ "\n")
    else ()
  end
  ; check_fexp_simple trace env delta pot e tp ext mode isSend

and synth_fexp_simple' trace env delta pot (e : A.parsed_expr)  ext mode isSend =
  begin
    if trace
    then print_string ("Synth: [" ^ PP.pp_mode mode ^ "] : " ^  PP.pp_fexp env 0 (e.A.func_structure) ^ "\n")
    else ()
  end
  ; synth_fexp_simple trace env delta pot e ext mode isSend

and check_fexp_simple trace env delta pot (e : A.parsed_expr) tp ext mode isSend = match (e.A.func_structure) with
    A.If(e1,e2,e3) ->
      begin
        let (delta1, pot1) = check_fexp_simple' trace env delta pot e1 A.Boolean ext mode isSend in
        let (delta2, pot2) = check_fexp_simple' trace env delta1 pot1 e2 tp ext mode isSend in
        let (delta3, pot3) = check_fexp_simple' trace env delta1 pot1 e3 tp ext mode isSend in
        min_pot (delta2, pot2) (delta3, pot3)
      end
  | A.LetIn(x,e1,e2) ->
      begin
        let (delta1, pot1, t) = synth_fexp_simple trace env delta pot e1 ext mode isSend in
        let (delta2, pot2) = check_fexp_simple' trace env (add_var (x,t) delta1) pot1 e2 tp ext mode isSend in
        (remove_var x delta2, pot2)
      end
  | A.Bool(_) ->
      begin
        match tp with
            A.Boolean -> (delta, pot)
          | _ -> error (e.A.func_data) ("type mismatch of " ^ PP.pp_fexp env 0 (e.A.func_structure) ^ ": expected boolean, found: " ^ PP.pp_ftp_simple tp)
      end
  | A.Int(_) ->
      begin
        match tp with
            A.Integer -> (delta, pot)
          | _ -> error (e.A.func_data) ("type mismatch of " ^ PP.pp_fexp env 0 (e.A.func_structure) ^ ": expected integer, found: " ^ PP.pp_ftp_simple tp)
      end
  | A.Addr(_) ->
      begin
        match tp with
            A.Address -> (delta, pot)
          | _ -> error (e.A.func_data) ("type mismatch of " ^ PP.pp_fexp env 0 (e.A.func_structure) ^ ": expected address, found: " ^ PP.pp_ftp_simple tp)

      end
  | A.Var(x) ->
      begin
        let t1 = lookup_ftp x delta (e.A.func_data) in
        if (eq_ftp tp t1)
        then
          let delta' = if isSend then consume x delta else delta in
          (delta', pot)
        else error (e.A.func_data) ("type mismatch of " ^ PP.pp_fexp env 0 (e.A.func_structure) ^ " : " ^ PP.pp_ftp_simple t1 ^ " <> " ^ PP.pp_ftp_simple tp)
      end
  | A.ListE(l) ->
      begin
        let cons_exp = consify l in
        check_fexp_simple' trace env delta pot {func_structure = cons_exp; func_data = e.A.func_data} tp ext mode isSend
      end
  | A.Op(e1, _, e2) ->
      begin
        if not(eq_ftp tp A.Integer) then error (e.A.func_data) ("type mismatch of " ^ PP.pp_fexp env 0 (e.A.func_structure) ^ ", expected int, found: " ^ (PP.pp_ftp_simple tp))
        else
          let (delta1, pot1) = check_fexp_simple' trace env delta pot e1 A.Integer ext mode isSend in
          let (delta2, pot2) = check_fexp_simple' trace env delta1 pot1 e2 A.Integer ext mode isSend in
          (delta2, pot2)
      end
  | A.CompOp(e1, _, e2) ->
      begin
        if not(eq_ftp tp A.Boolean) then error (e.A.func_data) ("type mismatch of " ^ PP.pp_fexp env 0 (e.A.func_structure) ^ ", expected boolean, found: " ^ (PP.pp_ftp_simple tp))
        else
          let (delta1, pot1) = check_fexp_simple' trace env delta pot e1 A.Integer ext mode isSend in
          let (delta2, pot2) = check_fexp_simple' trace env delta1 pot1 e2 A.Integer ext mode isSend in
          (delta2, pot2)
      end
  | A.EqAddr(e1, e2) ->
      begin
        if not(eq_ftp tp A.Boolean) then error (e.A.func_data) ("type mismatch of " ^ PP.pp_fexp env 0 (e.A.func_structure) ^ ", expected boolean, found: " ^ (PP.pp_ftp_simple tp))
        else
          let (delta1, pot1) = check_fexp_simple' trace env delta pot e1 A.Address ext mode isSend in
          let (delta2, pot2) = check_fexp_simple' trace env delta1 pot1 e2 A.Address ext mode isSend in
          (delta2, pot2)
      end
  | A.RelOp(e1, _, e2) ->
      begin
        if not(eq_ftp tp A.Boolean) then error (e.A.func_data) ("type mismatch of " ^ PP.pp_fexp env 0 (e.A.func_structure) ^ ", expected boolean, found: " ^ (PP.pp_ftp_simple tp))
        else
          let (delta1, pot1) = check_fexp_simple' trace env delta pot e1 A.Boolean ext mode isSend in
          let (delta2, pot2) = check_fexp_simple' trace env delta1 pot1 e2 A.Boolean ext mode isSend in
          (delta2, pot2)
      end
  | A.Cons(e1, e2) ->
      begin
        match tp with
          A.ListTP(t', pot') ->
          let (delta1, pot1) = check_fexp_simple' trace env delta pot e1 t' ext mode isSend in
          let (delta2, pot2) = check_fexp_simple' trace env delta1 pot1 e2 tp ext mode isSend in
          if not(ge pot2 pot') then error (e.A.func_data) ("insufficient potential for cons: " ^ PP.pp_fexp env 0 (e.A.func_structure) ^ " : " ^ pp_lt pot2 pot')
          else (delta2, minus pot2 pot')
        | _ -> error (e.A.func_data) ("type mismatch of " ^ PP.pp_fexp env 0 (e.A.func_structure) ^ ", expected list, found: " ^ PP.pp_ftp_simple tp)
      end
  | A.Match(e1, e2, x, xs, e3) ->
      begin
        let (delta1, pot1, t) = synth_fexp_simple' trace env delta pot e1 ext mode isSend in
        match t with
            A.ListTP(t', pot') ->
              let (delta2, pot2) = check_fexp_simple' trace env delta1 pot1 e2 tp ext mode isSend in
              let (delta3, pot3) = check_fexp_simple' trace env (add_var (x, t') (add_var (xs, t) delta2)) (plus pot2 pot') e3 tp ext mode isSend in
              (remove_var x (remove_var xs delta3), pot3)
          | _ -> error (e.A.func_data) ("type mismatch of " ^ PP.pp_fexp env 0 e1.A.func_structure ^ ", expected list, found: " ^ PP.pp_ftp_simple t)
      end
  | A.Lambda(args, e') ->
      begin
        match tp, args with
            A.Arrow(t1, t2), A.Single(x, _) ->
              let (delta1, pot1) = check_fexp_simple' trace env (add_var (x, t1) delta) pot e' t2 ext mode isSend in
              (delta1, pot1)
          | A.Arrow(t1, t2), A.Curry((x, ext'), xs) -> check_fexp_simple' trace env (add_var (x, t1) delta) pot ({func_structure = A.Lambda(xs, e'); func_data = ext'}) t2 ext mode isSend
          | _  -> error (e.A.func_data) ("type mismatch of " ^ PP.pp_fexp env 0 (e.A.func_structure) ^ ", expected arrow, found: " ^ PP.pp_ftp_simple tp)
      end
  | A.App(l) ->
      begin
        let (l', en) = A.split_last l in
        let (delta1, pot1, t) =
          begin
            if List.length l' > 1
            then synth_fexp_simple' trace env delta pot ({func_structure = A.App(l'); func_data = e.A.func_data}) ext mode isSend
            else synth_fexp_simple' trace env delta pot (List.hd l') ext mode isSend
          end
        in
        match t with
            A.Arrow(t1,t2) ->
              if not (eq_ftp t2 tp)
              then error (e.A.func_data) ("type mismatch of " ^ PP.pp_fexp env 0 (e.A.func_structure) ^ ", expected: " ^ PP.pp_ftp_simple tp ^ ", found: " ^ PP.pp_ftp_simple t2)
              else
                let (delta2, pot2) = check_fexp_simple' trace env delta1 pot1 en t1 ext mode isSend in
                (delta2, pot2)
          | _t -> error (e.A.func_data) ("type mismatch of " ^ PP.pp_fexp env 0 (A.App(l')) ^ ", expected arrow, found: " ^ PP.pp_ftp_simple t)
      end
  | A.Tick(cpot,e1) ->
      begin
        if not (ge pot cpot)
        then error (e.A.func_data) ("insufficient potential to tick: " ^ pp_lt pot cpot)
        else
          let (delta1, pot1) = (delta, minus pot cpot) in
          let (delta2, pot2) = check_fexp_simple' trace env delta1 pot1 e1 tp ext mode isSend in
          (delta2, pot2)
      end
  | A.GetTxnNum ->
      begin
        match tp with
            A.Integer -> (delta, pot)
          | _ -> error (e.A.func_data) ("type mismatch of " ^ PP.pp_fexp env 0 (e.A.func_structure) ^ ": expected integer, found: " ^ PP.pp_ftp_simple tp)
      end
  | A.GetTxnSender ->
      begin
        match tp with
            A.Address -> (delta, pot)
          | _ -> error (e.A.func_data) ("type mismatch of " ^ PP.pp_fexp env 0 (e.A.func_structure) ^ ": expected address, found: " ^ PP.pp_ftp_simple tp)
      end
  | A.Command _ -> raise UnknownTypeError

and synth_fexp_simple trace env delta pot (e : A.parsed_expr) ext mode isSend = match (e.A.func_structure) with
    A.If(e1,e2,e3) ->
      begin
        let (delta1, pot1) = check_fexp_simple' trace env delta pot e1 A.Boolean ext mode isSend in
        let (delta2, pot2, t) = synth_fexp_simple' trace env delta1 pot1 e2 ext mode isSend in
        let (delta3, pot3) = check_fexp_simple' trace env delta1 pot1 e3 t ext mode isSend in
        let (odelta, opot) = min_pot (delta2, pot2) (delta3, pot3) in
        (odelta, opot, t)
      end
  | A.LetIn _ -> error (e.A.func_data) ("cannot synthesize type of " ^ PP.pp_fexp env 0 (e.A.func_structure))
  | A.Bool _ -> (delta, pot, A.Boolean)
  | A.Int _ -> (delta, pot, A.Integer)
  | A.Addr _ -> (delta, pot, A.Address)
  | A.Var(x) ->
      begin
        let t = lookup_ftp x delta (e.A.func_data) in
        let delta' = if isSend then consume x delta else delta in
        (delta', pot, t)
      end
  | A.ListE(l) ->
      begin
        if List.length l = 0
        then error (e.A.func_data) ("cannot synthesize type of empty list: " ^ PP.pp_fexp env 0 (e.A.func_structure))
        else
          let cons_exp = consify l in
          synth_fexp_simple' trace env delta pot {func_structure = cons_exp; func_data = e.A.func_data} ext mode isSend
      end
  | A.Cons(e1,e2) ->
      begin
        let (delta2, pot2, tplist) = synth_fexp_simple' trace env delta pot e2 ext mode isSend in
        match tplist with
            A.ListTP(tp,_pot) ->
              let (delta1, pot1) = check_fexp_simple' trace env delta2 pot2 e1 tp ext mode isSend in
              (delta1, pot1, tplist)
          | _t -> error (e.A.func_data) ("type of " ^ PP.pp_fexp env 0 e2.A.func_structure ^ " not a list")
      end
  | A.Match _ -> error (e.A.func_data) ("cannot synthesize type of " ^ PP.pp_fexp env 0 (e.A.func_structure))
  | A.Lambda _ -> error (e.A.func_data) ("cannot synthesize type of " ^ PP.pp_fexp env 0 (e.A.func_structure))
  | A.Op(e1, _, e2) ->
      begin
        let (delta1, pot1) = check_fexp_simple' trace env delta pot e1 A.Integer ext mode isSend in
        let (delta2, pot2) = check_fexp_simple' trace env delta1 pot1 e2 A.Integer ext mode isSend in
        (delta2, pot2, A.Integer)
      end
  | A.CompOp(e1, _, e2) ->
      begin
        let (delta1, pot1) = check_fexp_simple' trace env delta pot e1 A.Integer ext mode isSend in
        let (delta2, pot2) = check_fexp_simple' trace env delta1 pot1 e2 A.Integer ext mode isSend in
        (delta2, pot2, A.Boolean)
      end
  | A.EqAddr(e1, e2) ->
      begin
        let (delta1, pot1) = check_fexp_simple' trace env delta pot e1 A.Address ext mode isSend in
        let (delta2, pot2) = check_fexp_simple' trace env delta1 pot1 e2 A.Address ext mode isSend in
        (delta2, pot2, A.Boolean)
      end
  | A.RelOp(e1, _, e2) ->
      begin
        let (delta1, pot1) = check_fexp_simple' trace env delta pot e1 A.Boolean ext mode isSend in
        let (delta2, pot2) = check_fexp_simple' trace env delta1 pot1 e2 A.Boolean ext mode isSend in
        (delta2, pot2, A.Boolean)
      end
  | A.App(l) ->
      begin
        let (l', en) = A.split_last l in
        let (delta1, pot1, t) =
          begin
            if List.length l' > 1
            then synth_fexp_simple' trace env delta pot ({func_structure=A.App(l'); func_data = (e.A.func_data)}) ext mode isSend
            else synth_fexp_simple' trace env delta pot ((List.hd l')) ext mode isSend
          end
        in
        match t with
            A.Arrow(t1,t2) ->
              let (delta2, pot2) = check_fexp_simple' trace env delta1 pot1 en t1 ext mode isSend in
              (delta2, pot2, t2)
          | _t -> error (e.A.func_data) ("type mismatch of " ^ PP.pp_fexp env 0 (A.App(l')) ^ ", expected arrow, found: " ^ PP.pp_ftp_simple t)
      end
  | A.Tick(cpot,e1) ->
      begin
        if not (ge pot cpot)
        then error (e.A.func_data) ("insufficient potential to tick: " ^ pp_lt pot cpot)
        else
          let (delta1, pot1) = (delta, minus pot cpot) in
          let (delta2, pot2, t) = synth_fexp_simple' trace env delta1 pot1 e1 ext mode isSend in
          (delta2, pot2, t)
      end
  | A.GetTxnNum -> (delta, pot, A.Integer)
  | A.GetTxnSender -> (delta, pot, A.Address)
  | A.Command _ -> error (e.A.func_data) ("cannot synthesize type of " ^ PP.pp_fexp env 0 (e.A.func_structure))


and checkfexp trace env delta pot e zc ext mode = match e.A.func_structure with
    A.Command(p) -> check_exp' trace env delta pot p zc ext mode
  | _ -> error (e.A.func_data) ("only command allowed at outermost declaration")

and check_exp' trace env delta pot p zc ext mode =
  begin
    if trace
    then print_string ("[" ^ PP.pp_mode mode ^ "] : " ^  PP.pp_exp_prefix (p.A.st_structure) ^ " : "
                          ^ PP.pp_tpj_compact env delta pot zc ^ "\n")
    else ()
  end
  ; check_exp trace env delta pot p zc ext mode


 (* judgmental constructs: id, cut, spawn, call *)
and check_exp trace env delta pot exp zc ext mode = match (exp.A.st_structure) with
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
              if eqtp env a c
              then ()
              else error (exp.A.st_data) ("left type " ^ PP.pp_tp_compact env a ^ " not equal to right type " ^
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
            else if eqtp env a c
            then ()
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
              if not (ge pot lpot)
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
                check_exp' trace env (add_chan env (x,a') delta') (minus pot lpot) q zc ext mode
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
              else if not (eqtp env a' c)
              then error (exp.A.st_data) ("type mismatch on right, expected: " ^ PP.pp_tp_compact env a' ^
                              ", found: " ^ PP.pp_tp_compact env c)
              else
                let ctx = join ctx in
                let delta' = match_ctx env ctx xs delta (List.length ctx) (List.length xs) (exp.A.st_data) in
                if List.length delta'.linear <> 0
                then error (exp.A.st_data) ("unconsumed channel(s) from linear context: " ^ PP.pp_lsctx env delta'.linear)
                else ()
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
                A.TpName(v) -> check_exp' trace env delta pot exp (z,A.expd_tp env v) ext mode
              | A.Plus(choices) ->
                  begin
                    match A.lookup_choice choices k with
                        None -> E.error_label_invalid env (k,c,z) (exp.A.st_data)
                      | Some ck -> check_exp' trace env delta pot p (z,ck) ext mode
                  end
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
              A.TpName(v) -> check_exp' trace env (update_tp env x (A.expd_tp env v) delta) pot exp zc ext mode
            | A.With(choices) ->
                begin
                  match A.lookup_choice choices k with
                      None -> E.error_label_invalid env (k,a,x) (exp.A.st_data)
                    | Some ak -> check_exp' trace env (update_tp env x ak delta) pot p zc ext mode
                end
            | A.Plus _ | A.One
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
                A.TpName(v) -> check_exp' trace env delta pot exp (z,A.expd_tp env v) ext mode
              | A.With(choices) -> check_branchesR trace env delta pot branches z choices (exp.A.st_data) mode
              | A.Plus _ | A.One
              | A.Tensor _ | A.Lolli _
              | A.PayPot _ | A.GetPot _
              | A.Up _ | A.Down _
              | A.FArrow _ | A.FProduct _ ->
                error (exp.A.st_data) ("invalid type of " ^ PP.pp_chan z ^
                           ", expected external choice, found: " ^ PP.pp_tp_compact env c)
        else (* the type a of x must be internal choice *)
          let a = find_ltp x delta (exp.A.st_data) in
          match a with
              A.TpName(v) -> check_exp' trace env (update_tp env x (A.expd_tp env v) delta) pot exp zc ext mode
            | A.Plus(choices) -> check_branchesL trace env delta x choices pot branches zc (exp.A.st_data) mode
            | A.With _ | A.One
            | A.Tensor _ | A.Lolli _
            | A.PayPot _ | A.GetPot _
            | A.Up _ | A.Down _
            | A.FArrow _ | A.FProduct _ ->
              error (exp.A.st_data) ("invalid type of " ^ PP.pp_chan x ^
                         ", expected internal choice, found: " ^ PP.pp_tp_compact env a)
      end
  | A.Send(x,w,p) ->
      begin
        if not (check_tp w delta)
        then E.error_unknown_var_ctx (w) (exp.A.st_data)
        else if check_ltp w delta
        then
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
                    A.TpName(v) -> check_exp' trace env delta pot exp (z,A.expd_tp env v) ext mode
                  | A.Tensor(a,b,m) ->
                      let (_sw,_w,mw) = w in
                      if not (eqmode m mw)
                      then error (exp.A.st_data) ("mode mismatch, expected at tensor: " ^ PP.pp_mode m ^ ", found: " ^ PP.pp_chan w)
                      else if not (eqtp env a a')
                      then error (exp.A.st_data) ("type mismatch: type of " ^ PP.pp_chan w ^
                                      ", expected: " ^ PP.pp_tp_compact env a ^
                                      ", found: " ^ PP.pp_tp_compact env a')
                      else check_exp' trace env (remove_tp w delta) pot p (z,b) ext mode
                  | A.Plus _ | A.With _
                  | A.One | A.Lolli _
                  | A.PayPot _ | A.GetPot _
                  | A.Up _ | A.Down _
                  | A.FArrow _ | A.FProduct _ ->
                    error (exp.A.st_data) ("invalid type of " ^ PP.pp_chan x ^
                               ", expected tensor, found: " ^ PP.pp_tp_compact env c)
            else (* the type a of x must be lolli *)
              let d = find_ltp x delta (exp.A.st_data) in
              match d with
                  A.TpName(v) -> check_exp' trace env (update_tp env x (A.expd_tp env v) delta) pot exp zc ext mode
                | A.Lolli(a,b,m) ->
                    let (_sw,_w,mw) = w in
                    if not (eqmode m mw)
                    then error (exp.A.st_data) ("mode mismatch, expected at lolli: " ^ PP.pp_mode m ^ ", found: " ^ PP.pp_chan w)
                    else if not (eqtp env a a')
                    then error (exp.A.st_data) ("type mismatch: type of " ^ PP.pp_chan w ^
                                    ", expected: " ^ PP.pp_tp_compact env a ^
                                    ", found: " ^ PP.pp_tp_compact env a')
                    else check_exp' trace env (update_tp env x b (remove_tp w delta)) pot p zc ext mode
                | A.Plus _ | A.With _
                | A.One | A.Tensor _
                | A.PayPot _ | A.GetPot _
                | A.Up _ | A.Down _
                | A.FArrow _ | A.FProduct _ ->
                  error (exp.A.st_data) ("invalid type of " ^ PP.pp_chan x ^
                             ", expected lolli, found: " ^ PP.pp_tp_compact env d)
          end
        else
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
                    A.TpName(v) -> check_exp' trace env delta pot exp (z,A.expd_tp env v) ext mode
                  | A.Tensor(a,b,m) ->
                      let (_sw,_w,mw) = w in
                      if not (eqmode m mw)
                      then error (exp.A.st_data) ("mode mismatch, expected at tensor: " ^ PP.pp_mode m ^ ", found: " ^ PP.pp_chan w)
                      else if not (eqtp env a a')
                      then error (exp.A.st_data) ("type mismatch: type of " ^ PP.pp_chan w ^
                                      ", expected: " ^ PP.pp_tp_compact env a ^
                                      ", found: " ^ PP.pp_tp_compact env a')
                      else check_exp' trace env delta pot p (z,b) ext mode
                  | A.Plus _ | A.With _
                  | A.One | A.Lolli _
                  | A.PayPot _ | A.GetPot _
                  | A.Up _ | A.Down _
                  | A.FArrow _ | A.FProduct _ ->
                    error (exp.A.st_data) ("invalid type of " ^ PP.pp_chan x ^
                               ", expected tensor, found: " ^ PP.pp_tp_compact env c)
            else (* the type a of x must be lolli *)
              let d = find_ltp x delta (exp.A.st_data) in
              match d with
                  A.TpName(v) -> check_exp' trace env (update_tp env x (A.expd_tp env v) delta) pot exp zc ext mode
                | A.Lolli(a,b,m) ->
                    let (_sw,_w,mw) = w in
                    if not (eqmode m mw)
                    then error (exp.A.st_data) ("mode mismatch, expected at lolli: " ^ PP.pp_mode m ^ ", found: " ^ PP.pp_chan w)
                    else if not (eqtp env a a')
                    then error (exp.A.st_data) ("type mismatch: type of " ^ PP.pp_chan w ^
                                    ", expected: " ^ PP.pp_tp_compact env a ^
                                    ", found: " ^ PP.pp_tp_compact env a')
                    else check_exp' trace env (update_tp env x b delta) pot p zc ext mode
                | A.Plus _ | A.With _
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
                  A.TpName(v) -> check_exp' trace env delta pot exp (z,A.expd_tp env v) ext mode
                | A.Lolli(a,b,m) ->
                    let (_sy,_y,my) = y in
                    if not (eqmode m my)
                    then error (exp.A.st_data) ("mode mismatch, expected at lolli: " ^ PP.pp_mode m ^ ", found: " ^ PP.pp_chan y)
                    else if not (mode_recv m mode)
                    then error (exp.A.st_data) ("cannot receive at mode " ^ PP.pp_mode m ^ " when current mode is " ^ PP.pp_mode mode)
                    else check_exp' trace env (add_chan env (y,a) delta) pot p (z,b) ext mode
                | A.Plus _ | A.With _
                | A.One | A.Tensor _
                | A.PayPot _ | A.GetPot _
                | A.Up _ | A.Down _
                | A.FArrow _ | A.FProduct _ ->
                  error (exp.A.st_data) ("invalid type of " ^ PP.pp_chan x ^
                             ", expected lolli, found: " ^ PP.pp_tp_compact env c)
          else (* the type a of x must be tensor *)
            let d = find_ltp x delta (exp.A.st_data) in
            match d with
                A.TpName(v) -> check_exp' trace env (update_tp env x (A.expd_tp env v) delta) pot exp zc ext mode
              | A.Tensor(a,b,m) ->
                  let (_sy,_y,my) = y in
                  if not (eqmode m my)
                  then error (exp.A.st_data) ("mode mismatch, expected at tensor: " ^ PP.pp_mode m ^ ", found: " ^ PP.pp_chan y)
                  else if not (mode_recv m mode)
                  then error (exp.A.st_data) ("cannot receive at mode " ^ PP.pp_mode m ^ " when current mode is " ^ PP.pp_mode mode)
                  else check_exp' trace env (add_chan env (y,a) (update_tp env x b delta)) pot p zc ext mode
              | A.Plus _ | A.With _
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
          if not (eqtp env c A.One)
          then error (exp.A.st_data) ("type mismatch: type of " ^ PP.pp_chan x ^ ", expected: 1, " ^
                          "found: " ^ PP.pp_tp_compact env c)
          else ()
      end
  | A.Wait(x,p) ->
      begin
        if not (check_ltp x delta)
        then E.error_unknown_var (x) (exp.A.st_data)
        else
          let a = find_ltp x delta (exp.A.st_data) in
          if not (eqtp env a A.One)
          then error (exp.A.st_data) ("type mismatch: type of " ^ PP.pp_chan x ^ ", expected: 1, " ^
                          " found: " ^ PP.pp_tp_compact env a)
          else check_exp' trace env (remove_tp x delta) pot p zc ext mode
      end
  | A.Work(pot',p) ->
      begin
        if not (ge pot pot')
        then error (exp.A.st_data) ("insufficient potential to work: " ^ pp_lt pot pot')
        else if not (ge pot' zero)
        then error (exp.A.st_data) ("potential not positive: " ^ pp_lt pot' zero)
        else check_exp' trace env delta (minus pot pot') p zc ext mode
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
                A.TpName(v) -> check_exp' trace env delta pot exp (z,A.expd_tp env v) ext mode
              | A.PayPot(tpot,c') ->
                  if not (eq epot tpot)
                  then error (exp.A.st_data) ("potential mismatch: potential in type does not match " ^
                                  "potential in expression: " ^ pp_uneq epot tpot)
                  else if not (ge pot tpot)
                  then error (exp.A.st_data) ("insufficient potential to pay: " ^ pp_lt pot tpot)
                  else check_exp' trace env delta (minus pot tpot) p (z,c') ext mode
              | A.Plus _ | A.With _
              | A.Tensor _ | A.Lolli _
              | A.One | A.GetPot _
              | A.Up _ | A.Down _
              | A.FArrow _ | A.FProduct _ ->
              error (exp.A.st_data) ("invalid type of " ^ PP.pp_chan x ^
                                                ", expected paypot, found: " ^ PP.pp_tp_compact env c)
        else (* the type a of x must be getpot *)
          let a = find_ltp x delta (exp.A.st_data) in
          match a with
              A.TpName(v) -> check_exp' trace env (update_tp env x (A.expd_tp env v) delta) pot exp zc ext mode
            | A.GetPot(tpot,a') ->
                if not (eq epot tpot)
                then error (exp.A.st_data) ("potential mismatch: potential in type does not match " ^
                                "potential in expression: " ^ pp_uneq epot tpot)
                else if not (ge pot epot)
                then error (exp.A.st_data) ("insufficient potential to pay: " ^ pp_lt pot epot)
                else check_exp' trace env (update_tp env x a' delta) (minus pot epot) p zc ext mode
            | A.Plus _ | A.With _
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
                A.TpName(v) -> check_exp' trace env delta pot exp (z,A.expd_tp env v) ext mode
              | A.GetPot(tpot,c') ->
                  if not (eq epot tpot)
                  then error (exp.A.st_data) ("potential mismatch: potential in type does not match " ^
                                  "potential in expression: " ^ pp_uneq epot tpot)
                  else check_exp' trace env delta (plus pot epot) p (z,c') ext mode
              | A.Plus _ | A.With _
              | A.Tensor _ | A.Lolli _
              | A.One | A.PayPot _
              | A.Up _ | A.Down _
              | A.FArrow _ | A.FProduct _ -> error (exp.A.st_data) ("invalid type of " ^ PP.pp_chan x ^
                                                ", expected getpot, found: " ^ PP.pp_tp_compact env c)
        else (* the type a of x must be paypot *)
          let a = find_ltp x delta (exp.A.st_data) in
          match a with
              A.TpName(v) -> check_exp' trace env (update_tp env x (A.expd_tp env v) delta) pot exp zc ext mode
            | A.PayPot(tpot,a') ->
                if not (eq epot tpot)
                then error (exp.A.st_data) ("potential mismatch: potential in type does not match " ^
                                "potential in expression: " ^ pp_uneq epot tpot)
                else check_exp' trace env (update_tp env x a' delta) (plus pot epot) p zc ext mode
            | A.Plus _ | A.With _
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
              A.TpName(v) -> check_exp' trace env (update_tp env x (A.expd_tp env v) delta) pot exp zc ext mode
            | A.Up(a') -> check_exp' trace env (add_chan env (y,a') (remove_tp x delta)) pot p zc ext mode
            | A.Plus _ | A.With _
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
              A.TpName(v) -> check_exp' trace env delta pot exp (z,A.expd_tp env v) ext mode
            | A.Up(c') -> check_exp' trace env delta pot p (y,c') ext A.Linear
            | A.Plus _ | A.With _
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
              A.TpName(v) -> check_exp' trace env (update_tp env x (A.expd_tp env v) delta) pot exp zc ext mode
            | A.Down(a') -> check_exp' trace env (add_chan env (y,a') (remove_tp x delta)) pot p zc ext mode
            | A.Plus _ | A.With _
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
              A.TpName(v) -> check_exp' trace env delta pot exp (z,A.expd_tp env v) ext mode
            | A.Down(c') -> check_exp' trace env delta pot p (y,c') ext A.Shared
            | A.Plus _ | A.With _
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
                A.TpName(v) -> check_exp' trace env delta pot exp (z,A.expd_tp env v) ext mode
              | A.FProduct(t,b) ->
                  let (delta', pot') = check_fexp_simple trace env delta pot e t ext mode true in
                  check_exp' trace env delta' pot' p (z,b) ext mode
              | A.Plus _ | A.With _
              | A.One | A.Lolli _ | A.Tensor _
              | A.PayPot _ | A.GetPot _
              | A.Up _ | A.Down _
              | A.FArrow _ ->
                error (exp.A.st_data) ("invalid type of " ^ PP.pp_chan x ^
                            ", expected fproduct, found: " ^ PP.pp_tp_compact env c)
        else (* the type a of x must be farrow *)
          let d = find_ltp x delta (exp.A.st_data) in
          match d with
              A.TpName(v) -> check_exp' trace env (update_tp env x (A.expd_tp env v) delta) pot exp zc ext mode
            | A.FArrow(t,b) ->
                let (delta', pot') = check_fexp_simple trace env delta pot e t ext mode true in
                check_exp' trace env (update_tp env x b delta') pot' p zc ext mode
            | A.Plus _ | A.With _
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
                  A.TpName(v) -> check_exp' trace env delta pot exp (z,A.expd_tp env v) ext mode
                | A.FArrow(t,b) ->
                    check_exp' trace env (add_var (y,t) delta) pot p (z,b) ext mode
                | A.Plus _ | A.With _
                | A.One | A.Tensor _ | A.Lolli _
                | A.PayPot _ | A.GetPot _
                | A.Up _ | A.Down _
                | A.FProduct _ ->
                  error (exp.A.st_data) ("invalid type of " ^ PP.pp_chan x ^
                             ", expected farrow, found: " ^ PP.pp_tp_compact env c)
          else (* the type a of x must be fproduct *)
            let d = find_ltp x delta (exp.A.st_data) in
            match d with
                A.TpName(v) -> check_exp' trace env (update_tp env x (A.expd_tp env v) delta) pot exp zc ext mode
              | A.FProduct(t,b) ->
                  check_exp' trace env (add_var (y,t) (update_tp env x b delta)) pot p zc ext mode
              | A.Plus _ | A.With _
              | A.One | A.Lolli _ | A.Tensor _
              | A.PayPot _ | A.GetPot _
              | A.Up _ | A.Down _
              | A.FArrow _ ->
                error (exp.A.st_data) ("invalid type of " ^ PP.pp_chan x ^
                           ", expected fproduct, found: " ^ PP.pp_tp_compact env d)
      end
  | A.Let(x,e,p) ->
      begin
        let (delta', pot', t) = synth_fexp_simple trace env delta pot e ext mode false in
        check_exp' trace env (add_var (x,t) delta') pot' p zc ext mode
      end
  | A.IfS(e,p1,p2) ->
      begin
        let (delta', pot') = check_fexp_simple trace env delta pot e A.Boolean ext mode false in
        check_exp' trace env delta' pot' p1 zc ext mode;
        check_exp' trace env delta' pot' p2 zc ext mode
      end

and check_branchesR trace env delta pot branches z choices ext mode = match branches, choices with
    (l1,p)::branches', (l2,c)::choices' ->
      begin
        if trace then print_string ("| " ^ l1 ^ " => \n") else ()
        ; if l1 = l2 then () else E.error_label_mismatch (l1, l2) ext
        ; check_exp' trace env delta pot p (z,c) (p.A.st_data) mode
        ; check_branchesR trace env delta pot branches' z choices' ext mode
      end
  | [], [] -> ()
  | (l,_p)::_branches', [] ->
      E.error_label_missing_alt (l) ext
  | [], (l,_c)::_choices' ->
      E.error_label_missing_branch (l) ext

and check_branchesL trace env delta x choices pot branches zc ext mode = match choices, branches with
    (l1,a)::choices', (l2,p)::branches' ->
      begin
        if trace then print_string ("| " ^ l1 ^ " => \n") else ()
        ; if l1 = l2 then () else E.error_label_mismatch (l1, l2) ext
        ; check_exp' trace env (update_tp env x a delta) pot p zc (p.A.st_data) mode
        ; check_branchesL trace env delta x choices' pot branches' zc ext mode
      end
  | [], [] -> ()
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
