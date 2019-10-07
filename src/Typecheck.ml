(* Type Checking *)
(* Use the syntax-directed rules to check the types and
* raises ErrorMsg.Error if an error is discovered
*)

module R = Arith
module A = Ast
module PP = Pprint
module E = TpError

let error = ErrorMsg.error ErrorMsg.Type;;

(*********************)
(* Validity of types *)
(*********************)


(*
  Equi-Synchronizing Session Types
  Purely linear types are always equi-synchronizing
*)

let rec esync env seen tp c ext is_shared =
  if !Flags.verbosity > 1
  then print_string ("checking esync: \n" ^ PP.pp_tp env tp ^ "\n" ^ PP.pp_tp env c ^ "\n") ;
  match tp with
      A.Plus(choice) -> esync_choices env seen choice c ext is_shared
    | A.With(choice) -> esync_choices env seen choice c ext is_shared
    | A.Tensor(_a,b) -> esync env seen b c ext is_shared
    | A.Lolli(_a,b) -> esync env seen b c ext is_shared
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

  | _, A.Tensor(s,t) -> valid env pol s ext ; valid env Pos t ext
  | _, A.Lolli(s,t) -> valid env pol s ext ; valid env Neg t ext
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
  if !Flags.verbosity > 1
  then print_string ("comparing " ^ PP.pp_tp env a ^ " and " ^ PP.pp_tp env a' ^ "\n")
  else ()
  ; eq_tp env seen a a'

and eq_tp env seen tp tp' = match tp, tp' with
    A.Plus(choice), A.Plus(choice') ->
      eq_choice env seen choice choice'
  | A.With(choice), A.With(choice') ->
      eq_choice env seen choice choice'
  | A.Tensor(s,t), A.Tensor(s',t') ->
      eq_tp' env seen s s' && eq_tp' env seen t t'
  | A.Lolli(s,t), A.Lolli(s',t') ->
      eq_tp' env seen s s' && eq_tp' env seen t t'
  | A.One, A.One -> true

  | A.PayPot(A.Arith pot,a), A.PayPot(A.Arith pot',a') ->
      R.eq pot pot' && eq_tp' env seen a a'
  | A.PayPot(A.Star,_a), _ ->
      false
  | _, A.PayPot(A.Star,_a) ->
      false
  | A.GetPot(A.Star,_a), _ ->
      false
  | _, A.GetPot(A.Star,_a) ->
      false
  | A.GetPot(A.Arith pot,a), A.GetPot(A.Arith pot',a') -> 
      R.eq pot pot' && eq_tp' env seen a a'
  
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
(*                            
fun interactsL P = case P of
    A.CaseL _ => true | A.LabL _ => true | A.WaitL _ => true
  | A.WhenL _ => true | A.NowL _ => true
  | A.GetL _ => true | A.PayL _ => true
  | A.AssumeL _ => true | A.AssertL _ => true
  | A.Marked(marked_exp) => interactsL (Mark.data marked_exp)
  | _ => false

fun interactsR P = case P of
    A.CaseR _ => true | A.LabR _ => true | A.CloseR => true
  | A.WhenR _ => true | A.NowR _ => true
  | A.GetR _ => true | A.PayR _ => true
  | A.AssumeR _ => true | A.AssertR _ => true
  | A.Marked(marked_exp) => interactsR (Mark.data marked_exp)
  | _ => false*)

exception UnknownTypeError;;

let zero = A.Arith (R.Int 0);;

let eq pot1 pot2 = match pot1, pot2 with
    A.Star, A.Star
  | A.Star, A.Arith _
  | A.Arith _, A.Star -> true
  | A.Arith p1, A.Arith p2 -> R.eq p1 p2;;

let ge pot1 pot2 = match pot1, pot2 with
    A.Star, A.Star
  | A.Star, A.Arith _
  | A.Arith _, A.Star -> true
  | A.Arith p1, A.Arith p2 -> R.ge p1 p2;;

let minus pot1 pot2 = match pot1, pot2 with
    A.Star, A.Star
  | A.Star, A.Arith _
  | A.Arith _, A.Star -> A.Star
  | A.Arith p1, A.Arith p2 -> A.Arith (R.minus p1 p2);;

let minus pot1 pot2 = match pot1, pot2 with
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
| A.Arith p1, A.Arith p2 -> R.pp_uneq p1 p2;;

let chan_of (c, _tp) = c 
let tp_of (_c, tp) = tp;;

let rec checktp c delta = match delta with
    [] -> false
  | (x,_t)::delta' ->
      if x = c then true
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

let check_emp_lin delta =
  let {A.shared = _sdelta ; A.linear = ldelta ; A.ordered = _odelta} = delta in
  List.length ldelta = 0;;

(* must check for existence first *)
let rec findtp c delta = match delta with
      [] -> raise UnknownTypeError
    | (x,t)::delta' ->
        if x = c then t
        else findtp c delta';;

let find_stp c delta =
  let {A.shared = sdelta ; A.linear = _ldelta ; A.ordered = _odelta} = delta in
  findtp c sdelta;;

let find_ltp c delta = 
  let {A.shared = _sdelta ; A.linear = ldelta ; A.ordered = _odelta} = delta in
  findtp c ldelta;;

let rec removetp x delta = match delta with
    [] -> []
  | (y,t)::delta' ->
      if x = y
      then delta'
      else (y,t)::(removetp x delta');;

let remove_tp x delta =
  let {A.shared = sdelta ; A.linear = ldelta ; A.ordered = odelta} = delta in
  {A.shared = removetp x sdelta ; A.linear = removetp x ldelta ; A.ordered = removetp x odelta};;

let rec match_ctx env sig_ctx ctx delta sig_len len ext = match sig_ctx, ctx with
    (_sc,st)::sig_ctx', c::ctx' ->
      begin
        if not (check_tp c delta)
        then error ext ("unknown or duplicate variable: " ^ c)
        else
          let {A.shared = sdelta ; A.linear = _ldelta ; A.ordered = _odelta} = delta in
          if checktp c sdelta
          then
            begin
              let t = find_stp c delta in
              if eqtp env st t
              then match_ctx env sig_ctx' ctx' delta sig_len len ext
              else error ext ("shared type mismatch: type of " ^ c ^ " : " ^ PP.pp_tp_compact env t ^
                          " does not match type in declaration: " ^ PP.pp_tp_compact env st)
            end
          else
            begin
              let t = find_ltp c delta in
              if eqtp env st t
              then match_ctx env sig_ctx' ctx' (remove_tp c delta) sig_len len ext
              else error ext ("linear type mismatch: type of " ^ c ^ " : " ^ PP.pp_tp_compact env t ^
                          " does not match type in declaration: " ^ PP.pp_tp_compact env st)
            end
      end
  | [], [] -> delta
  | _, _ -> error ext ("process defined with " ^ string_of_int sig_len ^ 
            " arguments but called with " ^ string_of_int len ^ " arguments");;

let join delta =
  let {A.shared = _sdelta ; A.linear = _ldelta ; A.ordered = odelta} = delta in
  odelta;;

let add_chan env (x,a) delta =
  let {A.shared = sdelta ; A.linear = ldelta ; A.ordered = odelta} = delta in
  if A.is_shared env a
  then {A.shared = (x,a)::sdelta ; A.linear = ldelta ; A.ordered = (x,a)::odelta}
  else {A.shared = sdelta ; A.linear = (x,a)::ldelta ; A.ordered = (x,a)::odelta};;

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
let rec check_exp' trace env delta pot p zc ext =
  begin
    if trace
    then print_string (PP.pp_exp_prefix p ^ " : "
                          ^ PP.pp_tpj_compact env delta pot zc ^ "\n")
    else ()
  end
  ; check_exp trace env delta pot p zc ext


 (* judgmental constructs: id, cut, spawn, call *)
and check_exp trace env delta pot exp zc ext = match exp with
    A.Fwd(x,y) ->
      begin
        let {A.shared = sdelta ; A.linear = ldelta ; A.ordered = _odelta} = delta in
        let tx = chan_of zc in
        let () =
          if x <> tx
          then E.error_unknown_var_right (x,ext)
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
              let a = find_stp y delta in
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
            else if y <> ty
            then E.error_unknown_var_ctx (y,ext)
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
          | Some (ctx,lpot,(_x',a')) ->
              if not (ge pot lpot)
              then error ext ("insufficient potential to spawn: " ^ pp_lt pot lpot ext)
              else
                let ctx = join ctx in
                let delta' = match_ctx env ctx xs delta (List.length ctx) (List.length xs) ext in
                check_exp' trace env (add_chan env (x,a') delta') (minus pot lpot) q zc ext
      end
  | A.ExpName(x,f,xs) ->
      begin
        match A.lookup_expdec env f with
          None -> E.error_undeclared (f, ext)
        | Some (ctx,lpot,(_x',a')) ->
            if not (eq pot lpot)
            then error ext ("potential mismatch for tail call: " ^ pp_uneq pot lpot)
            else
              let (z,c) = zc in
              if x <> z
              then E.error_unknown_var_right (x,ext)
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
            match c with
                A.TpName(v) -> check_exp' trace env delta pot exp (z,A.expd_tp env v) ext
              | A.Plus(choices) ->
                  begin
                    match A.lookup_choice choices k with
                        None -> E.error_label_invalid env (k,c,z,ext)
                      | Some ck -> check_exp' trace env delta pot p (z,ck) ext
                  end
              | A.With _ | A.One
              | A.Tensor _ | A.Lolli _
              | A.PayPot _ | A.GetPot _
              | A.Up _ | A.Down _ ->
                error ext ("invalid type of " ^ z ^
                           ", expected internal choice, found: " ^ PP.pp_tp_compact env c)
        else (* the type a of x must be external choice *)
          let a = find_ltp x delta in
          match a with
              A.TpName(v) -> check_exp' trace env (A.update_tp x (A.expd_tp env v) delta) pot exp zc ext
            | A.With(choices) ->
                begin
                  match A.lookup_choice choices k with
                      None -> E.error_label_invalid env (k,a,x,ext)
                    | Some ak -> check_exp' trace env (A.update_tp x ak delta) pot p zc ext
                end
            | A.Plus _ | A.One
            | A.Tensor _ | A.Lolli _
            | A.PayPot _ | A.GetPot _
            | A.Up _ | A.Down _ ->
              error ext ("invalid type of " ^ x ^
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
            match c with
                A.TpName(v) -> check_exp' trace env delta pot exp (z,A.expd_tp env v) ext
              | A.With(choices) -> check_branchesR trace env delta pot branches z choices ext
              | A.Plus _ | A.One
              | A.Tensor _ | A.Lolli _
              | A.PayPot _ | A.GetPot _
              | A.Up _ | A.Down _ ->
                error ext ("invalid type of " ^ z ^
                           ", expected external choice, found: " ^ PP.pp_tp_compact env c)
        else (* the type a of x must be internal choice *)
          let a = find_ltp x delta in
          match a with
              A.TpName(v) -> check_exp' trace env (A.update_tp x (A.expd_tp env v) delta) pot exp zc ext
            | A.Plus(choices) -> check_branchesL trace env delta x choices pot branches zc ext
            | A.With _ | A.One
            | A.Tensor _ | A.Lolli _
            | A.PayPot _ | A.GetPot _
            | A.Up _ | A.Down _ ->
              error ext ("invalid type of " ^ x ^
                         ", expected internal choice, found: " ^ PP.pp_tp_compact env a)
      end
  | A.Send(x,w,p) ->
      begin
        if not (check_tp w delta)
        then E.error_unknown_var_ctx (w,ext)
        else if check_ltp w delta
        then
          begin
            let a' = find_ltp w delta in
            if not (check_tp x delta)
            then
              if not (checktp x [zc])
              then E.error_unknown_var (x,ext)
              else (* the type c of z must be tensor *)
                let (z,c) = zc in
                match c with
                    A.TpName(v) -> check_exp' trace env delta pot exp (z,A.expd_tp env v) ext
                  | A.Tensor(a,b) ->
                      if not (eqtp env a a')
                      then error ext ("type mismatch: type of " ^ w ^
                                      ", expected: " ^ PP.pp_tp_compact env a ^
                                      ", found: " ^ PP.pp_tp_compact env a')
                      else check_exp' trace env (remove_tp w delta) pot p (z,b) ext
                  | A.Plus _ | A.With _
                  | A.One | A.Lolli _
                  | A.PayPot _ | A.GetPot _
                  | A.Up _ | A.Down _ ->
                    error ext ("invalid type of " ^ x ^
                               ", expected tensor, found: " ^ PP.pp_tp_compact env c)
            else (* the type a of x must be lolli *)
              let d = find_ltp x delta in
              match d with
                  A.TpName(v) -> check_exp' trace env (A.update_tp x (A.expd_tp env v) delta) pot exp zc ext
                | A.Lolli(a,b) ->
                    if not (eqtp env a a')
                    then error ext ("type mismatch: type of " ^ w ^
                                    ", expected: " ^ PP.pp_tp_compact env a ^
                                    ", found: " ^ PP.pp_tp_compact env a')
                    else check_exp' trace env (A.update_tp x b (remove_tp w delta)) pot p zc ext
                | A.Plus _ | A.With _
                | A.One | A.Tensor _
                | A.PayPot _ | A.GetPot _
                | A.Up _ | A.Down _ ->
                  error ext ("invalid type of " ^ x ^
                             ", expected lolli, found: " ^ PP.pp_tp_compact env d)
          end
        else
          begin
            let a' = find_stp w delta in
            if not (check_tp x delta)
            then
              if not (checktp x [zc])
              then E.error_unknown_var (x,ext)
              else (* the type c of z must be tensor *)
                let (z,c) = zc in
                match c with
                    A.TpName(v) -> check_exp' trace env delta pot exp (z,A.expd_tp env v) ext
                  | A.Tensor(a,b) ->
                      if not (eqtp env a a')
                      then error ext ("type mismatch: type of " ^ w ^
                                      ", expected: " ^ PP.pp_tp_compact env a ^
                                      ", found: " ^ PP.pp_tp_compact env a')
                      else check_exp' trace env delta pot p (z,b) ext
                  | A.Plus _ | A.With _
                  | A.One | A.Lolli _
                  | A.PayPot _ | A.GetPot _
                  | A.Up _ | A.Down _ ->
                    error ext ("invalid type of " ^ x ^
                               ", expected tensor, found: " ^ PP.pp_tp_compact env c)
            else (* the type a of x must be lolli *)
              let d = find_ltp x delta in
              match d with
                  A.TpName(v) -> check_exp' trace env (A.update_tp x (A.expd_tp env v) delta) pot exp zc ext
                | A.Lolli(a,b) ->
                    if not (eqtp env a a')
                    then error ext ("type mismatch: type of " ^ w ^
                                    ", expected: " ^ PP.pp_tp_compact env a ^
                                    ", found: " ^ PP.pp_tp_compact env a')
                    else check_exp' trace env (A.update_tp x b delta) pot p zc ext
                | A.Plus _ | A.With _
                | A.One | A.Tensor _
                | A.PayPot _ | A.GetPot _
                | A.Up _ | A.Down _ ->
                  error ext ("invalid type of " ^ x ^
                             ", expected lolli, found: " ^ PP.pp_tp_compact env d)
          end
      end
  | A.Recv(x,y,p) ->
      begin
        if check_tp y delta || checktp y [zc]
        then error ext ("variable " ^ y ^ " is not fresh")
        else
          if not (check_ltp x delta)
          then
            if not (checktp x [zc])
            then E.error_unknown_var (x,ext)
            else (* the type c of z must be lolli *)
              let (z,c) = zc in
              match c with
                  A.TpName(v) -> check_exp' trace env delta pot exp (z,A.expd_tp env v) ext
                | A.Lolli(a,b) -> check_exp' trace env (add_chan env (y,a) delta) pot p (z,b) ext
                | A.Plus _ | A.With _
                | A.One | A.Tensor _
                | A.PayPot _ | A.GetPot _
                | A.Up _ | A.Down _ ->
                  error ext ("invalid type of " ^ x ^
                             ", expected lolli, found: " ^ PP.pp_tp_compact env c)
          else (* the type a of x must be tensor *)
            let d = find_ltp x delta in
            match d with
                A.TpName(v) -> check_exp' trace env (A.update_tp x (A.expd_tp env v) delta) pot exp zc ext
              | A.Tensor(a,b) -> check_exp' trace env (add_chan env (y,a) (A.update_tp x b delta)) pot p zc ext
              | A.Plus _ | A.With _
              | A.One | A.Lolli _
              | A.PayPot _ | A.GetPot _
              | A.Up _ | A.Down _ ->
                error ext ("invalid type of " ^ x ^
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
        then error ext ("unconsumed potential: " ^ R.pp_uneq pot zero)
        else
          let (z,c) = zc in
          if not (eqtp env c A.One)
          then error ext ("type mismatch: type of " ^ z ^ ", expected: 1, " ^
                          "found: " ^ PP.pp_tp_compact env c)
          else ()
      end
  | A.Wait(x,p) ->
      begin
        if not (check_ltp x delta)
        then E.error_unknown_var (x,ext)
        else
          let a = find_ltp x delta in
          if not (eqtp env a A.One)
          then error ext ("type mismatch: type of " ^ x ^ ", expected: 1, " ^
                          " found: " ^ PP.pp_tp_compact env a)
          else check_exp' trace env (remove_tp x delta) pot p zc ext
      end
  | A.Work(pot',p) ->
      begin
        if not (ge pot pot')
        then error ext ("insufficient potential to work: " ^ pp_lt pot pot')
        else if not (ge pot' zero)
        then error ext ("potential not positive: " ^ R.pp_lt pot' zero)
        else check_exp' trace env delta (minus pot pot') p zc ext
      end
  | A.Pay(x,epot,p) ->
      begin
        if not (check_ltp x delta)
        then
          if not (checktp x [zc])
          then E.error_unknown_var (x,ext)
          else
            let (z,c) = zc in
            match c with
                A.TpName(v) -> check_exp' trace env delta pot exp (z,A.expd_tp env v) ext
              | A.PayPot(tpot,c') ->
                  if not (eq epot tpot)
                  then error ext ("potential mismatch: potential in type does not match " ^
                                  "potential in expression: " ^ R.pp_uneq epot tpot)
                  else if not (ge pot tpot)
                  then error ext ("insufficient potential to pay: " ^ R.pp_lt pot tpot)
                  else check_exp' trace env delta (minus pot tpot) p (z,c') ext
              | A.Plus _ | A.With _
              | A.Tensor _ | A.Lolli _
              | A.One | A.GetPot _
              | A.Up _ | A.Down _ -> error ext ("invalid type of " ^ x ^
                                                ", expected paypot, found: " ^ PP.pp_tp_compact env c)
        else
          let a = find_ltp x delta in
          match a with
              A.TpName(v) -> check_exp' trace env (A.update_tp x (A.expd_tp env v) delta) pot exp zc ext
            | A.GetPot(tpot,a') ->
                if not (eq epot tpot)
                then error ext ("potential mismatch: potential in type does not match " ^
                                "potential in expression: " ^ pp_uneq epot tpot)
                else if not (ge pot tpot)
                then error ext ("insufficient potential to pay: " ^ pp_lt pot tpot)
                else check_exp' trace env (A.update_tp x a' delta) (minus pot tpot) p zc ext
            | A.Plus _ | A.With _
            | A.Tensor _ | A.Lolli _
            | A.One | A.PayPot _
            | A.Up _ | A.Down _ -> error ext ("invalid type of " ^ x ^
                                              ", expected getpot, found: " ^ PP.pp_tp_compact env a)
      end
  | A.Get(x,epot,p) ->
      begin
        if not (check_ltp x delta)
        then
          if not (checktp x [zc])
          then E.error_unknown_var (x,ext)
          else
            let (z,c) = zc in
            match c with
                A.TpName(v) -> check_exp' trace env delta pot exp (z,A.expd_tp env v) ext
              | A.GetPot(tpot,c') ->
                  if not (eq epot tpot)
                  then error ext ("potential mismatch: potential in type does not match " ^
                                  "potential in expression: " ^ pp_uneq epot tpot)
                  else check_exp' trace env delta (R.plus pot tpot) p (z,c') ext
              | A.Plus _ | A.With _
              | A.Tensor _ | A.Lolli _
              | A.One | A.PayPot _
              | A.Up _ | A.Down _ -> error ext ("invalid type of " ^ x ^
                                                ", expected getpot, found: " ^ PP.pp_tp_compact env c)
        else
          let a = find_ltp x delta in
          match a with
              A.TpName(v) -> check_exp' trace env (A.update_tp x (A.expd_tp env v) delta) pot exp zc ext
            | A.PayPot(tpot,a') ->
                if not (eq epot tpot)
                then error ext ("potential mismatch: potential in type does not match " ^
                                "potential in expression: " ^ pp_uneq epot tpot)
                else check_exp' trace env (A.update_tp x a' delta) (plus pot tpot) p zc ext
            | A.Plus _ | A.With _
            | A.Tensor _ | A.Lolli _
            | A.One | A.GetPot _
            | A.Up _ | A.Down _ -> error ext ("invalid type of " ^ x ^
                                              ", expected paypot, found: " ^ PP.pp_tp_compact env a)
      end
  | A.Acquire(x,y,p) ->
      begin
        if check_tp y delta || checktp y [zc]
        then error ext ("variable " ^ y ^ " is not fresh")
        else if not (check_stp x delta)
        then E.error_unknown_var_ctx (x,ext)
        else
          let a = find_stp x delta in
          match a with
              A.TpName(v) -> check_exp' trace env (A.update_tp x (A.expd_tp env v) delta) pot exp zc ext
            | A.Up(a') -> check_exp' trace env (add_chan env (y,a') (remove_tp x delta)) pot p zc ext
            | A.Plus _ | A.With _
            | A.Tensor _ | A.Lolli _
            | A.One
            | A.PayPot _ | A.GetPot _
            | A.Down _ -> error ext ("invalid type of " ^ x ^
                                     ", expected up, found: " ^ PP.pp_tp_compact env a)
      end
  | A.Accept(x,y,p) ->
      begin
        if check_tp y delta || checktp y [zc]
        then error ext ("variable " ^ y ^ " is not fresh")
        else if not (checktp x [zc])
        then E.error_unknown_var_right (x,ext)
        else if not (check_emp_lin delta)
        then error ext ("independence principle violated: " ^
                        "expected empty linear context, found: " ^
                        PP.pp_lsctx env delta.linear)
        else
          let (z,c) = zc in
          match c with
              A.TpName(v) -> check_exp' trace env delta pot exp (z,A.expd_tp env v) ext
            | A.Up(c') -> check_exp' trace env delta pot p (y,c') ext
            | A.Plus _ | A.With _
            | A.Tensor _ | A.Lolli _
            | A.One
            | A.PayPot _ | A.GetPot _
            | A.Down _ -> error ext ("invalid type of " ^ x ^
                                     ", expected up, found: " ^ PP.pp_tp_compact env c)
      end
  | A.Release(x,y,p) ->
      begin
        if check_tp y delta || checktp y [zc]
        then error ext ("variable " ^ y ^ " is not fresh")
        else if not (check_ltp x delta)
        then E.error_unknown_var_ctx (x,ext)
        else
          let a = find_ltp x delta in
          match a with
              A.TpName(v) -> check_exp' trace env (A.update_tp x (A.expd_tp env v) delta) pot exp zc ext
            | A.Down(a') -> check_exp' trace env (add_chan env (y,a') (remove_tp x delta)) pot p zc ext
            | A.Plus _ | A.With _
            | A.Tensor _ | A.Lolli _
            | A.One
            | A.PayPot _ | A.GetPot _
            | A.Up _ -> error ext ("invalid type of " ^ x ^
                                     ", expected down, found: " ^ PP.pp_tp_compact env a)
      end
  | A.Detach(x,y,p) ->
      begin
        if check_tp y delta || checktp y [zc]
        then error ext ("variable " ^ y ^ " is not fresh")
        else if not (checktp x [zc])
        then E.error_unknown_var_right (x,ext)
        else if not (check_emp_lin delta)
        then error ext ("independence principle violated: " ^
                        "expected empty linear context, found: " ^
                        PP.pp_lsctx env delta.linear)
        else
          let (z,c) = zc in
          match c with
              A.TpName(v) -> check_exp' trace env delta pot exp (z,A.expd_tp env v) ext
            | A.Down(c') -> check_exp' trace env delta pot p (y,c') ext
            | A.Plus _ | A.With _
            | A.Tensor _ | A.Lolli _
            | A.One
            | A.PayPot _ | A.GetPot _
            | A.Up _ -> error ext ("invalid type of " ^ x ^
                                     ", expected down, found: " ^ PP.pp_tp_compact env c)
      end
  | A.Marked(marked_P) ->
      check_exp trace env delta pot (Mark.data marked_P) zc (Mark.ext marked_P)


and check_branchesR trace env delta pot branches z choices ext = match branches, choices with
    {lab_exp = (l1,p); exp_extent = bext}::branches', (l2,c)::choices' ->
      begin
        if trace then print_string ("| " ^ l1 ^ " => \n") else ()
        ; if l1 = l2 then () else E.error_label_mismatch (l1, l2, bext)
        ; check_exp' trace env delta pot p (z,c) ext
        ; check_branchesR trace env delta pot branches' z choices' ext
      end
  | [], [] -> ()
  | {lab_exp = (l,_p); exp_extent = bext}::_branches', [] ->
      E.error_label_missing_alt (l, bext)
  | [], (l,_c)::_choices' ->
      E.error_label_missing_branch (l, ext)

and check_branchesL trace env delta x choices pot branches zc ext = match choices, branches with
    (l1,a)::choices', {lab_exp = (l2,p); exp_extent = bext}::branches' ->
      begin
        if trace then print_string ("| " ^ l1 ^ " => \n") else ()
        ; if l1 = l2 then () else E.error_label_mismatch (l1, l2, bext)
        ; check_exp' trace env (A.update_tp x a delta) pot p zc ext
        ; check_branchesL trace env delta x choices' pot branches' zc ext
      end
  | [], [] -> ()
  | [], {lab_exp = (l,_p); exp_extent = bext}::_branches' ->
      E.error_label_missing_alt (l,bext)
  | (l,_a)::_choices', [] ->
      E.error_label_missing_branch (l,ext);;

(* external interface *)
let checkexp = check_exp';;

(* structure TypeCheck *)
