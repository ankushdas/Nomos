(* Type Checking *)
(* Use the syntax-directed rules to check the types and
* raises ErrorMsg.Error if an error is discovered
*)

module R = Arith
module A = Ast
module PP = Pprint
module E = TpError
module I = Infer

let error = ErrorMsg.error ErrorMsg.Type;;

(*********************)
(* Validity of types *)
(*********************)


(*
  Equi-Synchronizing Session Types
  Purely linear types are always equi-synchronizing
*)

let rec esync env seen tp c ext is_shared =
  if !Flags.verbosity >= 3
  then print_string ("checking esync: \n" ^ PP.pp_tp env tp ^ "\n" ^ PP.pp_tp env c ^ "\n") ;
  match tp with
      A.Plus(choice) -> esync_choices env seen choice c ext is_shared
    | A.With(choice) -> esync_choices env seen choice c ext is_shared
    | A.Tensor(_a,b,_m) -> esync env seen b c ext is_shared
    | A.Lolli(_a,b,_m) -> esync env seen b c ext is_shared
    | A.One -> if is_shared then error ext ("type not equi-synchronizing") else ()
    | A.PayPot(_pot,a) -> esync env seen a c ext is_shared
    | A.GetPot(_pot,a) -> esync env seen a c ext is_shared
    | A.TpName(v) ->
        if List.exists (fun x -> x = v) seen
        then ()
        else esync env (v::seen) (A.expd_tp env v) c ext is_shared
    | A.Up(a) -> esync env seen a c ext true
    | A.Down(a) -> esync env seen a c ext false
  
  and esync_choices env seen cs c ext is_shared = match cs with
      (_l,a)::as' -> esync env seen a c ext is_shared ; esync_choices env seen as' c ext is_shared
    | [] -> ();;

let esync_tp env tp ext = esync env [] tp tp ext false;;

(* Occurrences of |> and <| are restricted to
* positive and negative positions in a type, respectively
*)
type polarity = Pos | Neg | Zero;;

(* valid env ctx con polarity A ext = ()
* raises ErrorMsg.Error if not a valid type
* env must be the full environment which checking any
* type to allow mutually recursive definitions
* Type A must be an actual type (not '.' = A.Dot)
*)
let rec valid env pol tp ext = match pol, tp with
    _, A.Plus(choice) -> valid_choice env Pos choice ext
  | _, A.With(choice) -> valid_choice env Neg choice ext

  | _, A.Tensor(s,t,_m) -> valid env pol s ext ; valid env Pos t ext
  | _, A.Lolli(s,t,_m) -> valid env pol s ext ; valid env Neg t ext
  | _, A.One -> ()

  | _, A.Up(a) -> valid env pol a ext
  | _, A.Down(a) -> valid env pol a ext

  | Pos, A.PayPot(A.Arith pot,a) ->
      if not (R.non_neg pot) (* allowing 0, for uniformity *)
      then error ext ("potential " ^ PP.pp_arith pot ^ " not positive")
      else valid env Pos a ext
  | Pos, A.PayPot(A.Star,a) ->
      valid env Pos a ext
  | Neg, A.PayPot(_,_) -> error ext ("|> appears in a negative context")
  | Zero, A.PayPot(_,_) -> error ext ("|> appears in a neutral context")

  | Pos, A.GetPot(_,_a) -> error ext ("<| appears in a positive context")
  | Zero, A.GetPot(_,_a) -> error ext ("<| appears in a neutral context")
  | Neg, A.GetPot(A.Arith pot,a) ->
      if not (R.non_neg pot) (* allowing 0, for uniformity *)
      then error ext ("potential " ^ PP.pp_arith pot ^ " not positive")
      else valid env Neg a ext
  | Neg, A.GetPot(A.Star,a) ->
      valid env Neg a ext

  | _, A.TpName(a) ->
    (* allow forward references since 'env' is the full environment *)
    match A.lookup_tp env a with
        None -> error ext ("type name " ^ a ^ " undefined")
      | Some (_) -> ()
and valid_choice env pol cs ext = match cs with
    [] -> ()
  | (_l,al)::choices ->
    valid env pol al ext
    ; valid_choice env pol choices ext;;

(***********************)
(* Properties of types *)
(***********************)

let contractive tp = match tp with
    A.TpName(_a) -> false
  | A.Plus _ | A.With _
  | A.Tensor _ | A.Lolli _
  | A.One
  | A.PayPot _ | A.GetPot _
  | A.Up _ | A.Down _ -> true;;

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

let mode_L (_c,m) = match m with
    A.Linear
  | A.Unknown -> true
  | A.Var v -> I.m_eq_const v A.Linear
  | _ -> false;;

let mode_S (_c,m) = match m with
    A.Shared
  | A.Unknown -> true
  | A.Var v -> I.m_eq_const v A.Shared
  | _ -> false;;

let mode_P (_c,m) = match m with
    A.Pure
  | A.Unknown -> true
  | A.Var v -> I.m_eq_const v A.Pure
  | _ -> false;;

let mode_T (_c,m) = match m with
    A.Transaction
  | A.Unknown -> true
  | A.Var v -> I.m_eq_const v A.Transaction
  | _ -> false;;

let mode_lin (_c,m) = match m with
    A.Pure
  | A.Linear
  | A.Transaction
  | A.Unknown -> true
  | A.Var v -> I.m_lin v
  | _ -> false;;

let eqmode m1 m2 = match m1, m2 with
    A.Pure, A.Pure
  | A.Linear, A.Linear
  | A.Transaction, A.Transaction
  | A.Shared, A.Shared
  | A.Unknown, _
  | _, A.Unknown -> true
  | A.Var v1, A.Var v2 -> I.m_eq v1 v2
  | A.Var v, _ -> I.m_eq_const v m2
  | _, A.Var v -> I.m_eq_const v m1
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
  | A.Var v, A.Pure -> I.m_eq_const v A.Pure
  | _, A.Pure -> false
  | A.Pure, A.Shared
  | A.Shared, A.Shared -> true
  | A.Var v, A.Shared -> I.m_eq_pair v A.Pure A.Shared
  | _, A.Shared -> false
  | _, A.Linear -> true
  | _, A.Transaction -> true
  | _, _ -> false;;

let rec mem_env env a a' = match env with
    {A.declaration = A.TpEq(A.TpName(b),A.TpName(b')); decl_extent = _ext}::env' ->
      if b = a && b' = a' then true
    else if b = a' && b' = a then true   (* flip! *)
    else mem_env env' a a'
  | _decl::env' -> mem_env env' a a'
  | [] -> false

let rec mem_seen env seen a a' = match seen with
    (b,b')::seen' ->
      if b = a && b' = a' then true
      else if b = a' && b' = a
      then true
      else mem_seen env seen' a a'
  | [] -> mem_env env a a'

(* eq_tp env con seen A A' = true if (A = A'), defined coinductively *)
let rec eq_tp' env seen a a' =
  if !Flags.verbosity >= 3
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
let name_of (c,_m) = c;;

let eq_name (c1,_m1) (c2,_m2) = c1 = c2;;

let eq_mode (_c1,m1) (_c2,m2) = eqmode m1 m2;;

let eq_chan c1 c2 = 
  if eq_name c1 c2 && eq_mode c1 c2
  then true
  else false;;

let rec checktp c delta = match delta with
    [] -> false
  | (x,_t)::delta' ->
      if eq_name x c then true
      else checktp c delta';;

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

let find_stp c delta ext =
  let {A.shared = sdelta ; A.linear = _ldelta ; A.ordered = _odelta} = delta in
  if not (mode_S c)
  then error ext ("mode of channel " ^ PP.pp_chan c ^ " not S")
  else findtp c sdelta ext;;

let find_ltp c delta ext = 
  let {A.shared = _sdelta ; A.linear = ldelta ; A.ordered = _odelta} = delta in
  if not (mode_lin c)
  then E.error_mode_shared_comm (c, ext)
  else findtp c ldelta ext;;

let rec removetp x delta = match delta with
    [] -> []
  | (y,t)::delta' ->
      if eq_name x y
      then delta'
      else (y,t)::(removetp x delta');;

let remove_tp x delta =
  let {A.shared = sdelta ; A.linear = ldelta ; A.ordered = odelta} = delta in
  {A.shared = removetp x sdelta ; A.linear = removetp x ldelta ; A.ordered = removetp x odelta};;

let add_chan env (x,a) delta =
  let {A.shared = sdelta ; A.linear = ldelta ; A.ordered = odelta} = delta in
  if A.is_shared env a
  then {A.shared = (x,a)::sdelta ; A.linear = ldelta ; A.ordered = (x,a)::odelta}
  else {A.shared = sdelta ; A.linear = (x,a)::ldelta ; A.ordered = (x,a)::odelta};;

let update_tp env x t delta =
  let delta = remove_tp x delta in
  add_chan env (x,t) delta;;

let rec match_ctx env sig_ctx ctx delta sig_len len ext = match sig_ctx, ctx with
    (sc,st)::sig_ctx', c::ctx' ->
      begin
        if not (check_tp c delta)
        then error ext ("unknown or duplicate variable: " ^ PP.pp_chan c)
        else if not (eq_mode sc c)
        then E.error_mode_mismatch (sc, c, ext)
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
  | [], [] -> delta
  | _, _ -> error ext ("process defined with " ^ string_of_int sig_len ^ 
            " arguments but called with " ^ string_of_int len ^ " arguments");;

let join delta =
  let {A.shared = _sdelta ; A.linear = _ldelta ; A.ordered = odelta} = delta in
  odelta;;

(* check_exp trace env ctx con A pot P C = () if A |{pot}- P : C
* raises ErrorMsg.Error otherwise
* assumes ctx ; con |= A valid
*         ctx ; con |= C valid
*         ctx ; con |= pot nat
*
* trace = true means to print some tracing information
*
* entry point is check_exp'
*
* We expand type definitions lazily, based on the direction of
* interactions.  This is done so tracing (if enabled) or error
* message are more intelligible.
*)
let rec check_exp' trace env delta pot p zc ext mode =
  begin
    if trace
    then print_string ("[" ^ PP.pp_mode mode ^ "] : " ^  PP.pp_exp_prefix p ^ " : "
                          ^ PP.pp_tpj_compact env delta pot zc ^ "\n")
    else ()
  end
  ; check_exp trace env delta pot p zc ext mode


 (* judgmental constructs: id, cut, spawn, call *)
and check_exp trace env delta pot exp zc ext mode = match exp with
    A.Fwd(x,y) ->
      begin
        let {A.shared = sdelta ; A.linear = ldelta ; A.ordered = _odelta} = delta in
        let tx = chan_of zc in
        let () =
          if not (eq_name x tx)
          then E.error_unknown_var_right (x,ext)
          else if not (eq_mode x tx)
          then E.error_mode_mismatch (x, tx, ext)
          else if not (eq_mode x y)
          then error ext ("mode mismatch: " ^ PP.pp_chan x ^ " != " ^ PP.pp_chan y)
          else ()
        in
        let c = tp_of zc in
        if A.is_shared env c
        then
          begin
            if List.length sdelta = 0
            then error ext ("shared context empty while offered channel is shared")
            else if not (checktp y sdelta)
            then E.error_unknown_var_ctx (y,ext)
            else
              let a = find_stp y delta ext in
              if eqtp env a c
              then ()
              else error ext ("left type " ^ PP.pp_tp_compact env a ^ " not equal to right type " ^
              PP.pp_tp_compact env c)
          end
        else
          begin
            let (ty, a) = List.hd ldelta in
            if List.length ldelta <> 1
            then error ext ("linear context " ^ PP.pp_lsctx env ldelta ^ " must have only one channel")
            else if not (eq_name y ty)
            then E.error_unknown_var_ctx (y,ext)
            else if not (eq_mode y ty)
            then E.error_mode_mismatch (y, ty, ext)
            else if not (eq pot zero)
            then error ext ("unconsumed potential: " ^ pp_uneq pot zero)
            else if eqtp env a c
            then ()
            else error ext ("left type " ^ PP.pp_tp_compact env a ^ " not equal to right type " ^
                      PP.pp_tp_compact env c)
          end
      end
  | A.Spawn(x,f,xs,q) ->
      begin
        match A.lookup_expdec env f with
            None -> E.error_undeclared (f, ext)
          | Some (ctx,lpot,(x',a'),mdef) ->
              let (_x,mx) = x in
              if not (ge pot lpot)
              then error ext ("insufficient potential to spawn: " ^ pp_lt pot lpot)
              else if not (eq_mode x x')
              then E.error_mode_mismatch (x, x', ext)
              else if not (eqmode mx mdef)
              then error ext ("mode mismatch: expected " ^ PP.pp_mode mdef ^ " at declaration, found: " ^ PP.pp_chan x)
              else if not (mode_spawn mdef mode)
              then error ext ("cannot spawn at mode " ^ PP.pp_mode mdef ^ " when current mode is " ^ PP.pp_mode mode)
              else
                let ctx = join ctx in
                let delta' = match_ctx env ctx xs delta (List.length ctx) (List.length xs) ext in
                check_exp' trace env (add_chan env (x,a') delta') (minus pot lpot) q zc ext mode
      end
  | A.ExpName(x,f,xs) ->
      begin
        match A.lookup_expdec env f with
          None -> E.error_undeclared (f, ext)
        | Some (ctx,lpot,(x',a'),mdef) ->
            let (_x,mx) = x in
            if not (eq pot lpot)
            then error ext ("potential mismatch for tail call: " ^ pp_uneq pot lpot)
            else if not (eq_mode x x')
            then E.error_mode_mismatch (x, x', ext)
            else if not (eqmode mx mdef)
            then error ext ("mode mismatch: expected " ^ PP.pp_mode mdef ^ " at declaration, found: " ^ PP.pp_chan x)
            else if not (mode_spawn mdef mode)
            then error ext ("cannot tail call at mode " ^ PP.pp_mode mdef ^ " when current mode is " ^ PP.pp_mode mode)
            else
              let (z,c) = zc in
              if not (eq_name x z)
              then E.error_unknown_var_right (x,ext)
              else if not (eq_mode x z)
              then E.error_mode_mismatch (x, z, ext)
              else if not (eqtp env a' c)
              then error ext ("type mismatch on right, expected: " ^ PP.pp_tp_compact env a' ^
                              ", found: " ^ PP.pp_tp_compact env c)
              else
                let ctx = join ctx in
                let delta' = match_ctx env ctx xs delta (List.length ctx) (List.length xs) ext in
                if List.length delta'.linear <> 0
                then error ext ("unconsumed channel(s) from linear context: " ^ PP.pp_lsctx env delta'.linear)
                else ()
      end
  | A.Lab(x,k,p) ->
      begin
        if not (check_ltp x delta)
        then
          if not (checktp x [zc])
          then E.error_unknown_var (x,ext)
          else (* the type c of z must be internal choice *)
            let (z,c) = zc in
            if not (eq_mode x z)
            then E.error_mode_mismatch (x, z, ext)
            else if not (mode_lin x)
            then E.error_mode_shared_comm (x, ext)
            else
            match c with
                A.TpName(v) -> check_exp' trace env delta pot exp (z,A.expd_tp env v) ext mode
              | A.Plus(choices) ->
                  begin
                    match A.lookup_choice choices k with
                        None -> E.error_label_invalid env (k,c,z,ext)
                      | Some ck -> check_exp' trace env delta pot p (z,ck) ext mode
                  end
              | A.With _ | A.One
              | A.Tensor _ | A.Lolli _
              | A.PayPot _ | A.GetPot _
              | A.Up _ | A.Down _ ->
                error ext ("invalid type of " ^ PP.pp_chan z ^
                           ", expected internal choice, found: " ^ PP.pp_tp_compact env c)
        else (* the type a of x must be external choice *)
          let a = find_ltp x delta ext in
          match a with
              A.TpName(v) -> check_exp' trace env (update_tp env x (A.expd_tp env v) delta) pot exp zc ext mode
            | A.With(choices) ->
                begin
                  match A.lookup_choice choices k with
                      None -> E.error_label_invalid env (k,a,x,ext)
                    | Some ak -> check_exp' trace env (update_tp env x ak delta) pot p zc ext mode
                end
            | A.Plus _ | A.One
            | A.Tensor _ | A.Lolli _
            | A.PayPot _ | A.GetPot _
            | A.Up _ | A.Down _ ->
              error ext ("invalid type of " ^ PP.pp_chan x ^
                         ", expected external choice, found: " ^ PP.pp_tp_compact env a)
      end
  | A.Case(x,branches) ->
      begin
        if not (check_ltp x delta)
        then
          if not (checktp x [zc])
          then E.error_unknown_var (x,ext)
          else (* the type c of z must be external choice *)
            let (z,c) = zc in
            if not (eq_mode x z)
            then E.error_mode_mismatch (x, z, ext)
            else if not (mode_lin x)
            then E.error_mode_shared_comm (x, ext)
            else
            match c with
                A.TpName(v) -> check_exp' trace env delta pot exp (z,A.expd_tp env v) ext mode
              | A.With(choices) -> check_branchesR trace env delta pot branches z choices ext mode
              | A.Plus _ | A.One
              | A.Tensor _ | A.Lolli _
              | A.PayPot _ | A.GetPot _
              | A.Up _ | A.Down _ ->
                error ext ("invalid type of " ^ PP.pp_chan z ^
                           ", expected external choice, found: " ^ PP.pp_tp_compact env c)
        else (* the type a of x must be internal choice *)
          let a = find_ltp x delta ext in
          match a with
              A.TpName(v) -> check_exp' trace env (update_tp env x (A.expd_tp env v) delta) pot exp zc ext mode
            | A.Plus(choices) -> check_branchesL trace env delta x choices pot branches zc ext mode
            | A.With _ | A.One
            | A.Tensor _ | A.Lolli _
            | A.PayPot _ | A.GetPot _
            | A.Up _ | A.Down _ ->
              error ext ("invalid type of " ^ PP.pp_chan x ^
                         ", expected internal choice, found: " ^ PP.pp_tp_compact env a)
      end
  | A.Send(x,w,p) ->
      begin
        if not (check_tp w delta)
        then E.error_unknown_var_ctx (w,ext)
        else if check_ltp w delta
        then
          begin
            let a' = find_ltp w delta ext in
            if not (check_tp x delta)
            then
              if not (checktp x [zc])
              then E.error_unknown_var (x,ext)
              else (* the type c of z must be tensor *)
                let (z,c) = zc in
                if not (eq_mode x z)
                then E.error_mode_mismatch (x, z, ext)
                else if not (mode_lin x)
                then E.error_mode_shared_comm (x, ext)
                else
                match c with
                    A.TpName(v) -> check_exp' trace env delta pot exp (z,A.expd_tp env v) ext mode
                  | A.Tensor(a,b,m) ->
                      let (_w,mw) = w in
                      if not (eqmode m mw)
                      then error ext ("mode mismatch, expected at tensor: " ^ PP.pp_mode m ^ ", found: " ^ PP.pp_chan w)
                      else if not (eqtp env a a')
                      then error ext ("type mismatch: type of " ^ PP.pp_chan w ^
                                      ", expected: " ^ PP.pp_tp_compact env a ^
                                      ", found: " ^ PP.pp_tp_compact env a')
                      else check_exp' trace env (remove_tp w delta) pot p (z,b) ext mode
                  | A.Plus _ | A.With _
                  | A.One | A.Lolli _
                  | A.PayPot _ | A.GetPot _
                  | A.Up _ | A.Down _ ->
                    error ext ("invalid type of " ^ PP.pp_chan x ^
                               ", expected tensor, found: " ^ PP.pp_tp_compact env c)
            else (* the type a of x must be lolli *)
              let d = find_ltp x delta ext in
              match d with
                  A.TpName(v) -> check_exp' trace env (update_tp env x (A.expd_tp env v) delta) pot exp zc ext mode
                | A.Lolli(a,b,m) ->
                    let (_w,mw) = w in
                    if not (eqmode m mw)
                    then error ext ("mode mismatch, expected at lolli: " ^ PP.pp_mode m ^ ", found: " ^ PP.pp_chan w)
                    else if not (eqtp env a a')
                    then error ext ("type mismatch: type of " ^ PP.pp_chan w ^
                                    ", expected: " ^ PP.pp_tp_compact env a ^
                                    ", found: " ^ PP.pp_tp_compact env a')
                    else check_exp' trace env (update_tp env x b (remove_tp w delta)) pot p zc ext mode
                | A.Plus _ | A.With _
                | A.One | A.Tensor _
                | A.PayPot _ | A.GetPot _
                | A.Up _ | A.Down _ ->
                  error ext ("invalid type of " ^ PP.pp_chan x ^
                             ", expected lolli, found: " ^ PP.pp_tp_compact env d)
          end
        else
          begin
            let a' = find_stp w delta ext in
            if not (check_tp x delta)
            then
              if not (checktp x [zc])
              then E.error_unknown_var (x,ext)
              else (* the type c of z must be tensor *)
                let (z,c) = zc in
                if not (eq_mode x z)
                then E.error_mode_mismatch (x, z, ext)
                else if not (mode_lin x)
                then E.error_mode_shared_comm (x, ext)
                else
                match c with
                    A.TpName(v) -> check_exp' trace env delta pot exp (z,A.expd_tp env v) ext mode
                  | A.Tensor(a,b,m) ->
                      let (_w,mw) = w in
                      if not (eqmode m mw)
                      then error ext ("mode mismatch, expected at tensor: " ^ PP.pp_mode m ^ ", found: " ^ PP.pp_chan w)
                      else if not (eqtp env a a')
                      then error ext ("type mismatch: type of " ^ PP.pp_chan w ^
                                      ", expected: " ^ PP.pp_tp_compact env a ^
                                      ", found: " ^ PP.pp_tp_compact env a')
                      else check_exp' trace env delta pot p (z,b) ext mode
                  | A.Plus _ | A.With _
                  | A.One | A.Lolli _
                  | A.PayPot _ | A.GetPot _
                  | A.Up _ | A.Down _ ->
                    error ext ("invalid type of " ^ PP.pp_chan x ^
                               ", expected tensor, found: " ^ PP.pp_tp_compact env c)
            else (* the type a of x must be lolli *)
              let d = find_ltp x delta ext in
              match d with
                  A.TpName(v) -> check_exp' trace env (update_tp env x (A.expd_tp env v) delta) pot exp zc ext mode
                | A.Lolli(a,b,m) ->
                    let (_w,mw) = w in
                    if not (eqmode m mw)
                    then error ext ("mode mismatch, expected at lolli: " ^ PP.pp_mode m ^ ", found: " ^ PP.pp_chan w)
                    else if not (eqtp env a a')
                    then error ext ("type mismatch: type of " ^ PP.pp_chan w ^
                                    ", expected: " ^ PP.pp_tp_compact env a ^
                                    ", found: " ^ PP.pp_tp_compact env a')
                    else check_exp' trace env (update_tp env x b delta) pot p zc ext mode
                | A.Plus _ | A.With _
                | A.One | A.Tensor _
                | A.PayPot _ | A.GetPot _
                | A.Up _ | A.Down _ ->
                  error ext ("invalid type of " ^ PP.pp_chan x ^
                             ", expected lolli, found: " ^ PP.pp_tp_compact env d)
          end
      end
  | A.Recv(x,y,p) ->
      begin
        if check_tp y delta || checktp y [zc]
        then error ext ("variable " ^ name_of y ^ " is not fresh")
        else
          if not (check_ltp x delta)
          then
            if not (checktp x [zc])
            then E.error_unknown_var (x,ext)
            else (* the type c of z must be lolli *)
              let (z,c) = zc in
              if not (eq_mode x z)
              then E.error_mode_mismatch (x, z, ext)
              else if not (mode_lin x)
              then E.error_mode_shared_comm (x, ext)
              else
              match c with
                  A.TpName(v) -> check_exp' trace env delta pot exp (z,A.expd_tp env v) ext mode
                | A.Lolli(a,b,m) ->
                    let (_y,my) = y in
                    if not (eqmode m my)
                    then error ext ("mode mismatch, expected at lolli: " ^ PP.pp_mode m ^ ", found: " ^ PP.pp_chan y)
                    else if not (mode_recv m mode)
                    then error ext ("cannot receive at mode " ^ PP.pp_mode m ^ " when current mode is " ^ PP.pp_mode mode)
                    else check_exp' trace env (add_chan env (y,a) delta) pot p (z,b) ext mode
                | A.Plus _ | A.With _
                | A.One | A.Tensor _
                | A.PayPot _ | A.GetPot _
                | A.Up _ | A.Down _ ->
                  error ext ("invalid type of " ^ PP.pp_chan x ^
                             ", expected lolli, found: " ^ PP.pp_tp_compact env c)
          else (* the type a of x must be tensor *)
            let d = find_ltp x delta ext in
            match d with
                A.TpName(v) -> check_exp' trace env (update_tp env x (A.expd_tp env v) delta) pot exp zc ext mode
              | A.Tensor(a,b,m) ->
                  let (_y,my) = y in
                  if not (eqmode m my)
                  then error ext ("mode mismatch, expected at tensor: " ^ PP.pp_mode m ^ ", found: " ^ PP.pp_chan y)
                  else if not (mode_recv m mode)
                  then error ext ("cannot receive at mode " ^ PP.pp_mode m ^ " when current mode is " ^ PP.pp_mode mode)
                  else check_exp' trace env (add_chan env (y,a) (update_tp env x b delta)) pot p zc ext mode
              | A.Plus _ | A.With _
              | A.One | A.Lolli _
              | A.PayPot _ | A.GetPot _
              | A.Up _ | A.Down _ ->
                error ext ("invalid type of " ^ PP.pp_chan x ^
                           ", expected tensor, found: " ^ PP.pp_tp_compact env d)
      end
  | A.Close(x) ->
      begin
        let {A.shared = _sdelta ; A.linear = ldelta ; A.ordered = _odelta} = delta in
        if List.length ldelta > 0
        then error ext ("linear context " ^ PP.pp_lsctx env ldelta ^ " not empty")
        else if not (checktp x [zc])
        then E.error_unknown_var (x,ext)
        else if not (eq pot zero)
        then error ext ("unconsumed potential: " ^ pp_uneq pot zero)
        else
          let (z,c) = zc in
          if not (eq_mode x z)
          then E.error_mode_mismatch (x, z, ext)
          else if not (mode_lin x)
          then E.error_mode_shared_comm (x, ext)
          else
          if not (eqtp env c A.One)
          then error ext ("type mismatch: type of " ^ PP.pp_chan x ^ ", expected: 1, " ^
                          "found: " ^ PP.pp_tp_compact env c)
          else ()
      end
  | A.Wait(x,p) ->
      begin
        if not (check_ltp x delta)
        then E.error_unknown_var (x,ext)
        else
          let a = find_ltp x delta ext in
          if not (eqtp env a A.One)
          then error ext ("type mismatch: type of " ^ PP.pp_chan x ^ ", expected: 1, " ^
                          " found: " ^ PP.pp_tp_compact env a)
          else check_exp' trace env (remove_tp x delta) pot p zc ext mode
      end
  | A.Work(pot',p) ->
      begin
        if not (ge pot pot')
        then error ext ("insufficient potential to work: " ^ pp_lt pot pot')
        else if not (ge pot' zero)
        then error ext ("potential not positive: " ^ pp_lt pot' zero)
        else check_exp' trace env delta (minus pot pot') p zc ext mode
      end
  | A.Pay(x,epot,p) ->
      begin
        if not (check_ltp x delta)
        then
          if not (checktp x [zc])
          then E.error_unknown_var (x,ext)
          else (* the type c of z must be paypot *)
            let (z,c) = zc in
            if not (eq_mode x z)
            then E.error_mode_mismatch (x, z, ext)
            else if not (mode_lin x)
            then E.error_mode_shared_comm (x, ext)
            else
            match c with
                A.TpName(v) -> check_exp' trace env delta pot exp (z,A.expd_tp env v) ext mode
              | A.PayPot(tpot,c') ->
                  if not (eq epot tpot)
                  then error ext ("potential mismatch: potential in type does not match " ^
                                  "potential in expression: " ^ pp_uneq epot tpot)
                  else if not (ge pot tpot)
                  then error ext ("insufficient potential to pay: " ^ pp_lt pot tpot)
                  else check_exp' trace env delta (minus pot tpot) p (z,c') ext mode
              | A.Plus _ | A.With _
              | A.Tensor _ | A.Lolli _
              | A.One | A.GetPot _
              | A.Up _ | A.Down _ -> error ext ("invalid type of " ^ PP.pp_chan x ^
                                                ", expected paypot, found: " ^ PP.pp_tp_compact env c)
        else (* the type a of x must be getpot *)
          let a = find_ltp x delta ext in
          match a with
              A.TpName(v) -> check_exp' trace env (update_tp env x (A.expd_tp env v) delta) pot exp zc ext mode
            | A.GetPot(tpot,a') ->
                if not (eq epot tpot)
                then error ext ("potential mismatch: potential in type does not match " ^
                                "potential in expression: " ^ pp_uneq epot tpot)
                else if not (ge pot epot)
                then error ext ("insufficient potential to pay: " ^ pp_lt pot epot)
                else check_exp' trace env (update_tp env x a' delta) (minus pot epot) p zc ext mode
            | A.Plus _ | A.With _
            | A.Tensor _ | A.Lolli _
            | A.One | A.PayPot _
            | A.Up _ | A.Down _ -> error ext ("invalid type of " ^ PP.pp_chan x ^
                                              ", expected getpot, found: " ^ PP.pp_tp_compact env a)
      end
  | A.Get(x,epot,p) ->
      begin
        if not (check_ltp x delta)
        then
          if not (checktp x [zc])
          then E.error_unknown_var (x,ext)
          else (* the type c of z must be getpot *)
            let (z,c) = zc in
            if not (eq_mode x z)
            then E.error_mode_mismatch (x, z, ext)
            else if not (mode_lin x)
            then E.error_mode_shared_comm (x, ext)
            else
            match c with
                A.TpName(v) -> check_exp' trace env delta pot exp (z,A.expd_tp env v) ext mode
              | A.GetPot(tpot,c') ->
                  if not (eq epot tpot)
                  then error ext ("potential mismatch: potential in type does not match " ^
                                  "potential in expression: " ^ pp_uneq epot tpot)
                  else check_exp' trace env delta (plus pot epot) p (z,c') ext mode
              | A.Plus _ | A.With _
              | A.Tensor _ | A.Lolli _
              | A.One | A.PayPot _
              | A.Up _ | A.Down _ -> error ext ("invalid type of " ^ PP.pp_chan x ^
                                                ", expected getpot, found: " ^ PP.pp_tp_compact env c)
        else (* the type a of x must be paypot *)
          let a = find_ltp x delta ext in
          match a with
              A.TpName(v) -> check_exp' trace env (update_tp env x (A.expd_tp env v) delta) pot exp zc ext mode
            | A.PayPot(tpot,a') ->
                if not (eq epot tpot)
                then error ext ("potential mismatch: potential in type does not match " ^
                                "potential in expression: " ^ pp_uneq epot tpot)
                else check_exp' trace env (update_tp env x a' delta) (plus pot epot) p zc ext mode
            | A.Plus _ | A.With _
            | A.Tensor _ | A.Lolli _
            | A.One | A.GetPot _
            | A.Up _ | A.Down _ -> error ext ("invalid type of " ^ PP.pp_chan x ^
                                              ", expected paypot, found: " ^ PP.pp_tp_compact env a)
      end
  | A.Acquire(x,y,p) ->
      begin
        if check_tp y delta || checktp y [zc]
        then error ext ("variable " ^ name_of y ^ " is not fresh")
        else if not (check_stp x delta)
        then E.error_unknown_var_ctx (x,ext)
        else if not (mode_L y)
        then error ext ("mode mismatch of acquired channel: expected L, found " ^ PP.pp_chan y)
        else if not (mode_S x)
        then error ext ("mode mismatch of acquiring channel: expected S, found " ^ PP.pp_chan x)
        else
          let a = find_stp x delta ext in
          match a with
              A.TpName(v) -> check_exp' trace env (update_tp env x (A.expd_tp env v) delta) pot exp zc ext mode
            | A.Up(a') -> check_exp' trace env (add_chan env (y,a') (remove_tp x delta)) pot p zc ext mode
            | A.Plus _ | A.With _
            | A.Tensor _ | A.Lolli _
            | A.One
            | A.PayPot _ | A.GetPot _
            | A.Down _ -> error ext ("invalid type of " ^ PP.pp_chan x ^
                                     ", expected up, found: " ^ PP.pp_tp_compact env a)
      end
  | A.Accept(x,y,p) ->
      begin
        if check_tp y delta || checktp y [zc]
        then error ext ("variable " ^ name_of y ^ " is not fresh")
        else if not (checktp x [zc])
        then E.error_unknown_var_right (x,ext)
        else if not (mode_L y)
        then error ext ("mode mismatch of accepted channel: expected L, found " ^ PP.pp_chan y)
        else if not (mode_S x)
        then error ext ("mode mismatch of accepting channel: expected S, found " ^ PP.pp_chan x)
        else if not (purelin delta)
        then error ext ("independence principle violated: " ^
                        "expected pure linear context, found: " ^
                        PP.pp_lsctx env delta.linear)
        else
          let (z,c) = zc in
          if not (eq_mode x z)
          then E.error_mode_mismatch (x, z, ext)
          else
          match c with
              A.TpName(v) -> check_exp' trace env delta pot exp (z,A.expd_tp env v) ext mode
            | A.Up(c') -> check_exp' trace env delta pot p (y,c') ext A.Linear
            | A.Plus _ | A.With _
            | A.Tensor _ | A.Lolli _
            | A.One
            | A.PayPot _ | A.GetPot _
            | A.Down _ -> error ext ("invalid type of " ^ PP.pp_chan x ^
                                     ", expected up, found: " ^ PP.pp_tp_compact env c)
      end
  | A.Release(x,y,p) ->
      begin
        if check_tp y delta || checktp y [zc]
        then error ext ("variable " ^ name_of y ^ " is not fresh")
        else if not (check_ltp x delta)
        then E.error_unknown_var_ctx (x,ext)
        else if not (mode_S y)
        then error ext ("mode mismatch of released channel: expected S, found " ^ PP.pp_chan y)
        else if not (mode_L x)
        then error ext ("mode mismatch of releasing channel: expected L, found " ^ PP.pp_chan x)
        else
          let a = find_ltp x delta ext in
          match a with
              A.TpName(v) -> check_exp' trace env (update_tp env x (A.expd_tp env v) delta) pot exp zc ext mode
            | A.Down(a') -> check_exp' trace env (add_chan env (y,a') (remove_tp x delta)) pot p zc ext mode
            | A.Plus _ | A.With _
            | A.Tensor _ | A.Lolli _
            | A.One
            | A.PayPot _ | A.GetPot _
            | A.Up _ -> error ext ("invalid type of " ^ PP.pp_chan x ^
                                     ", expected down, found: " ^ PP.pp_tp_compact env a)
      end
  | A.Detach(x,y,p) ->
      begin
        if check_tp y delta || checktp y [zc]
        then error ext ("variable " ^ name_of y ^ " is not fresh")
        else if not (checktp x [zc])
        then E.error_unknown_var_right (x,ext)
        else if not (mode_S y)
        then error ext ("mode mismatch of detached channel: expected S, found " ^ PP.pp_chan y)
        else if not (mode_L x)
        then error ext ("mode mismatch of detaching channel: expected L, found " ^ PP.pp_chan x)
        else if not (purelin delta)
        then error ext ("independence principle violated: " ^
                        "expected empty linear context, found: " ^
                        PP.pp_lsctx env delta.linear)
        else
          let (z,c) = zc in
          if not (eq_mode x z)
          then E.error_mode_mismatch (x, z, ext)
          else
          match c with
              A.TpName(v) -> check_exp' trace env delta pot exp (z,A.expd_tp env v) ext mode
            | A.Down(c') -> check_exp' trace env delta pot p (y,c') ext A.Shared
            | A.Plus _ | A.With _
            | A.Tensor _ | A.Lolli _
            | A.One
            | A.PayPot _ | A.GetPot _
            | A.Up _ -> error ext ("invalid type of " ^ PP.pp_chan x ^
                                     ", expected down, found: " ^ PP.pp_tp_compact env c)
      end
  | A.Marked(marked_P) ->
      check_exp trace env delta pot (Mark.data marked_P) zc (Mark.ext marked_P) mode


and check_branchesR trace env delta pot branches z choices ext mode = match branches, choices with
    {lab_exp = (l1,p); exp_extent = bext}::branches', (l2,c)::choices' ->
      begin
        if trace then print_string ("| " ^ l1 ^ " => \n") else ()
        ; if l1 = l2 then () else E.error_label_mismatch (l1, l2, bext)
        ; check_exp' trace env delta pot p (z,c) ext mode
        ; check_branchesR trace env delta pot branches' z choices' ext mode
      end
  | [], [] -> ()
  | {lab_exp = (l,_p); exp_extent = bext}::_branches', [] ->
      E.error_label_missing_alt (l, bext)
  | [], (l,_c)::_choices' ->
      E.error_label_missing_branch (l, ext)

and check_branchesL trace env delta x choices pot branches zc ext mode = match choices, branches with
    (l1,a)::choices', {lab_exp = (l2,p); exp_extent = bext}::branches' ->
      begin
        if trace then print_string ("| " ^ l1 ^ " => \n") else ()
        ; if l1 = l2 then () else E.error_label_mismatch (l1, l2, bext)
        ; check_exp' trace env (update_tp env x a delta) pot p zc ext mode
        ; check_branchesL trace env delta x choices' pot branches' zc ext mode
      end
  | [], [] -> ()
  | [], {lab_exp = (l,_p); exp_extent = bext}::_branches' ->
      E.error_label_missing_alt (l,bext)
  | (l,_a)::_choices', [] ->
      E.error_label_missing_branch (l,ext);;

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
    | (x,_t)::odelta' ->
        let y = find_tp x sdelta ldelta in
        if not (eq_mode x y)
        then E.error_mode_mismatch (x, y, ext)
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

(* structure TypeCheck *)
