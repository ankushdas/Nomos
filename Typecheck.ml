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

  | Pos, A.PayPot(pot,a) ->
      if not (R.non_neg pot) (* allowing 0, for uniformity *)
      then error ext ("potential " ^ PP.pp_arith pot ^ " not positive")
      else valid env Pos a ext
  | Neg, A.PayPot(_,_) -> error ext ("|> appears in a negative context")
  | Zero, A.PayPot(_,_) -> error ext ("|> appears in a neutral context")

  | Pos, A.GetPot(_,_a) -> error ext ("<| appears in a positive context")
  | Zero, A.GetPot(_,_a) -> error ext ("<| appears in a neutral context")
  | Neg, A.GetPot(pot,a) ->
      if not (R.non_neg pot) (* allowing 0, for uniformity *)
      then error ext ("potential " ^ PP.pp_arith pot ^ " not positive")
      else valid env Neg a ext

  | _, A.TpName(a) ->
    (* allow forward references since 'env' is the full environment *)
    match A.lookup_tp env a with
        None -> error ext ("type name " ^ a ^ " undefined")
      | Some (_) -> ()
  (* A.Dot impossible *)
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
  | A.PayPot _ | A.GetPot _ -> true;;

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
  then print_string ("comparing " ^ A.pp_tp a ^ " and " ^ A.pp_tp a' ^ "\n")
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

  | A.PayPot(pot,a), A.PayPot(pot',a') ->
      R.eq pot pot' && eq_tp' env seen a a'
  | A.GetPot(pot,a), A.GetPot(pot',a') -> 
      R.eq pot pot' && eq_tp' env seen a a'

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

let is_tpname tp = match tp with
    A.TpName(_a) -> true
  | A.Plus _ | A.With _
  | A.Tensor _ | A.Lolli _
  | A.One
  | A.PayPot _ | A.GetPot _ -> false;;

let zero = R.Int(0);;

let chan_of (c, _tp) = c 
let tp_of (_c, tp) = tp;;

let rec check_tp c delta = match delta with
    [] -> false
  | (x,_t)::delta' ->
      if x = c then true
      else check_tp c delta';;

(* must check for existence first *)
let rec find_tp c delta = match delta with
      [] -> raise UnknownTypeError
    | (x,t)::delta' ->
        if x = c then t
        else find_tp c delta';;

let rec match_ctx env sig_ctx ctx delta sig_len len ext = match sig_ctx, ctx with
    (_sc,st)::sig_ctx', c::ctx' ->
      begin
        if not (check_tp c delta)
        then E.error_unknown_var_ctx (c,ext)
        else
          let t = find_tp c delta in
          if eqtp env st t
          then match_ctx env sig_ctx' ctx' delta sig_len len ext
          else error ext ("type mismatch: type of " ^ c ^ " : " ^ PP.pp_tp_compact env t ^
                          " does not match type in declaration: " ^ PP.pp_tp_compact env st)
      end
  | [], [] -> ()
  | _, _ -> error ext ("process defined with " ^ string_of_int sig_len ^ 
            " arguments but called with " ^ string_of_int len ^ " arguments");;

let rec remove_tp x delta = match delta with
    [] -> raise UnknownTypeError
  | (y,t)::delta' ->
      if x = y
      then delta'
      else (y,t)::(remove_tp x delta');;

let rec remove_tps xs delta = match xs with
    [] -> delta
  | x::xs' -> remove_tps xs' (remove_tp x delta);;

let rec consume_chans xs delta ext = match delta with
    [] -> ()
  | (x,_t)::delta' ->
      if List.mem x xs
      then consume_chans xs delta' ext
      else error ext ("unconsumed channel from context: " ^ x);;

let rec no_dups ctx ext = match ctx with
    [] -> ()
  | x::xs ->
      if List.mem x xs
      then error ext ("duplicate channel in call: " ^ x)
      else no_dups xs ext;;

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
and check_exp trace env delta pot exp zc ext = match delta, exp, zc with
    delta, A.Fwd(x,y), zc ->
      begin
        let a = tp_of (List.hd delta) in
        let c = tp_of zc in
        let ty = chan_of (List.hd delta) in
        let tx = chan_of zc in
        if (List.length delta <> 1)
        then error ext ("context " ^ A.pp_ctx delta ^ " must have only one channel")
        else if x <> tx
        then E.error_unknown_var_right (x,ext)
        else if y <> ty
        then E.error_unknown_var_ctx (y,ext)
        else if not (R.eq pot zero)
        then error ext ("unconsumed potential: " ^ R.pp_uneq pot zero)
        else if eqtp env a c
        then ()
        else error ext ("left type " ^ PP.pp_tp_compact env a ^ " not equal to right type " ^
                        PP.pp_tp_compact env c)
      end
  | delta, A.Spawn(x,f,xs,q), zc ->
      begin
        match A.lookup_expdec env f with
          None -> E.error_undeclared (f, ext)
        | Some (ctx,lpot,(_x',a')) ->
            if not (R.ge pot lpot)
            then error ext ("insufficient potential to spawn: " ^ R.pp_lt pot lpot)
            else
              let () = no_dups xs ext in
              let () = match_ctx env ctx xs delta (List.length ctx) (List.length xs) ext in
              check_exp' trace env ((x,a')::(remove_tps xs delta)) (R.minus pot lpot) q zc ext
      end
  | delta, A.ExpName(x,f,xs), zc ->
      begin
        match A.lookup_expdec env f with
          None -> E.error_undeclared (f, ext)
        | Some (ctx,lpot,(_x',a')) ->
            if not (R.eq pot lpot)
            then error ext ("potential mismatch for tail call: " ^ R.pp_uneq pot lpot)
            else
              let (z,c) = zc in
              if x <> z
              then E.error_unknown_var_right (x,ext)
              else if not (eqtp env a' c)
              then error ext ("type mismatch on right, expected: " ^ PP.pp_tp_compact env a' ^
                              ", found: " ^ PP.pp_tp_compact env c)
              else
                let () = consume_chans xs delta ext in
                let () = no_dups xs ext in
                match_ctx env ctx xs delta (List.length ctx) (List.length xs) ext
      end
  | delta, (A.Lab(x,k,p) as exp), zc ->
      begin
        if not (check_tp x delta)
        then
          if not (check_tp x [zc])
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
              | A.PayPot _ | A.GetPot _ ->
                error ext ("invalid type of " ^ z ^
                           ", expected internal choice, found: " ^ PP.pp_tp_compact env c)
        else (* the type a of x must be external choice *)
          let a = find_tp x delta in
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
            | A.PayPot _ | A.GetPot _ ->
              error ext ("invalid type of " ^ x ^
                         ", expected external choice, found: " ^ PP.pp_tp_compact env a)
      end
  | delta, (A.Case(x,branches) as exp), zc ->
      begin
        if not (check_tp x delta)
        then
          if not (check_tp x [zc])
          then E.error_unknown_var (x,ext)
          else (* the type c of z must be external choice *)
            let (z,c) = zc in
            match c with
                A.TpName(v) -> check_exp' trace env delta pot exp (z,A.expd_tp env v) ext
              | A.With(choices) -> check_branchesR trace env delta pot branches z choices ext
              | A.Plus _ | A.One
              | A.Tensor _ | A.Lolli _
              | A.PayPot _ | A.GetPot _ ->
                error ext ("invalid type of " ^ z ^
                           ", expected external choice, found: " ^ PP.pp_tp_compact env c)
        else (* the type a of x must be internal choice *)
          let a = find_tp x delta in
          match a with
              A.TpName(v) -> check_exp' trace env (A.update_tp x (A.expd_tp env v) delta) pot exp zc ext
            | A.Plus(choices) -> check_branchesL trace env delta x choices pot branches zc ext
            | A.With _ | A.One
            | A.Tensor _ | A.Lolli _
            | A.PayPot _ | A.GetPot _ ->
              error ext ("invalid type of " ^ x ^
                         ", expected internal choice, found: " ^ PP.pp_tp_compact env a)
      end
  | delta, (A.Send(x,w,p) as exp), zc ->
      begin
        if not (check_tp w delta)
        then E.error_unknown_var_ctx (w,ext)
        else
          let a' = find_tp w delta in
          if not (check_tp x delta)
          then
            if not (check_tp x [zc])
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
                | A.PayPot _ | A.GetPot _ ->
                  error ext ("invalid type of " ^ x ^
                             ", expected tensor, found: " ^ PP.pp_tp_compact env c)
          else (* the type a of x must be lolli *)
            let d = find_tp x delta in
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
              | A.PayPot _ | A.GetPot _ ->
                error ext ("invalid type of " ^ x ^
                           ", expected lolli, found: " ^ PP.pp_tp_compact env d)
      end
  | delta, (A.Recv(x,y,p) as exp), zc ->
      begin
        if check_tp y delta || check_tp y [zc]
        then error ext ("variable " ^ y ^ " is not fresh")
        else
          if not (check_tp x delta)
          then
            if not (check_tp x [zc])
            then E.error_unknown_var (x,ext)
            else (* the type c of z must be lolli *)
              let (z,c) = zc in
              match c with
                  A.TpName(v) -> check_exp' trace env delta pot exp (z,A.expd_tp env v) ext
                | A.Lolli(a,b) -> check_exp' trace env ((y,a)::delta) pot p (z,b) ext
                | A.Plus _ | A.With _
                | A.One | A.Tensor _
                | A.PayPot _ | A.GetPot _ ->
                  error ext ("invalid type of " ^ x ^
                             ", expected lolli, found: " ^ PP.pp_tp_compact env c)
          else (* the type a of x must be tensor *)
            let d = find_tp x delta in
            match d with
                A.TpName(v) -> check_exp' trace env (A.update_tp x (A.expd_tp env v) delta) pot exp zc ext
              | A.Tensor(a,b) -> check_exp' trace env ((y,a)::(A.update_tp x b delta)) pot p zc ext
              | A.Plus _ | A.With _
              | A.One | A.Lolli _
              | A.PayPot _ | A.GetPot _ ->
                error ext ("invalid type of " ^ x ^
                           ", expected tensor, found: " ^ PP.pp_tp_compact env d)
      end
  | delta, A.Close(x), zc ->
      begin
        if List.length delta > 0
        then error ext ("context " ^ A.pp_ctx delta ^ " not empty")
        else if not (check_tp x [zc])
        then E.error_unknown_var (x,ext)
        else if not (R.eq pot zero)
        then error ext ("unconsumed potential: " ^ R.pp_uneq pot zero)
        else
          let (z,c) = zc in
          if not (eqtp env c A.One)
          then error ext ("type mismatch: type of " ^ z ^ ", expected: 1, " ^
                          "found: " ^ PP.pp_tp_compact env c)
          else ()
      end
  | delta, A.Wait(x,p), zc ->
      begin
        if not (check_tp x delta)
        then E.error_unknown_var (x,ext)
        else
          let a = find_tp x delta in
          if not (eqtp env a A.One)
          then error ext ("type mismatch: type of " ^ x ^ ", expected: 1, " ^
                          " found: " ^ PP.pp_tp_compact env a)
          else check_exp' trace env (remove_tp x delta) pot p zc ext
      end
  | delta, A.Work(pot',p), zc ->
      begin
        if not (R.ge pot pot')
        then error ext ("insufficient potential to work: " ^ R.pp_lt pot pot')
        else if not (R.ge pot' zero)
        then error ext ("potential not positive: " ^ R.pp_lt pot' zero)
        else check_exp' trace env delta (R.minus pot pot') p zc ext
      end
  | delta, (A.Pay(x,epot,p) as exp), zc ->
      begin
        if not (check_tp x delta)
        then
          if not (check_tp x [zc])
          then E.error_unknown_var (x,ext)
          else
            let (z,c) = zc in
            match c with
                A.TpName(v) -> check_exp' trace env delta pot exp (z,A.expd_tp env v) ext
              | A.PayPot(tpot,c') ->
                  if not (R.eq epot tpot)
                  then error ext ("potential mismatch: potential in type does not match " ^
                                  "potential in expression: " ^ R.pp_uneq epot tpot)
                  else if not (R.ge pot tpot)
                  then error ext ("insufficient potential to pay: " ^ R.pp_lt pot tpot)
                  else check_exp' trace env delta (R.minus pot tpot) p (z,c') ext
              | A.Plus _ | A.With _
              | A.Tensor _ | A.Lolli _
              | A.One | A.GetPot _ -> error ext ("invalid type of " ^ x ^
                                            ", expected paypot, found: " ^ PP.pp_tp_compact env c)
        else
          let a = find_tp x delta in
          match a with
              A.TpName(v) -> check_exp' trace env (A.update_tp x (A.expd_tp env v) delta) pot exp zc ext
            | A.GetPot(tpot,a') ->
                if not (R.eq epot tpot)
                then error ext ("potential mismatch: potential in type does not match " ^
                                "potential in expression: " ^ R.pp_uneq epot tpot)
                else if not (R.ge pot tpot)
                then error ext ("insufficient potential to pay: " ^ R.pp_lt pot tpot)
                else check_exp' trace env (A.update_tp x a' delta) (R.minus pot tpot) p zc ext
            | A.Plus _ | A.With _
            | A.Tensor _ | A.Lolli _
            | A.One | A.PayPot _ -> error ext ("invalid type of " ^ x ^
                                          ", expected getpot, found: " ^ PP.pp_tp_compact env a)
      end
  | delta, (A.Get(x,epot,p) as exp), zc ->
      begin
        if not (check_tp x delta)
        then
          if not (check_tp x [zc])
          then E.error_unknown_var (x,ext)
          else
            let (z,c) = zc in
            match c with
                A.TpName(v) -> check_exp' trace env delta pot exp (z,A.expd_tp env v) ext
              | A.GetPot(tpot,c') ->
                  if not (R.eq epot tpot)
                  then error ext ("potential mismatch: potential in type does not match " ^
                                  "potential in expression: " ^ R.pp_uneq epot tpot)
                  else check_exp' trace env delta (R.plus pot tpot) p (z,c') ext
              | A.Plus _ | A.With _
              | A.Tensor _ | A.Lolli _
              | A.One | A.PayPot _ -> error ext ("invalid type of " ^ x ^
                                            ", expected getpot, found: " ^ PP.pp_tp_compact env c)
        else
          let a = find_tp x delta in
          match a with
              A.TpName(v) -> check_exp' trace env (A.update_tp x (A.expd_tp env v) delta) pot exp zc ext
            | A.PayPot(tpot,a') ->
                if not (R.eq epot tpot)
                then error ext ("potential mismatch: potential in type does not match " ^
                                "potential in expression: " ^ R.pp_uneq epot tpot)
                else check_exp' trace env (A.update_tp x a' delta) (R.plus pot tpot) p zc ext
            | A.Plus _ | A.With _
            | A.Tensor _ | A.Lolli _
            | A.One | A.GetPot _ -> error ext ("invalid type of " ^ x ^
                                          ", expected paypot, found: " ^ PP.pp_tp_compact env a)

      end
  | delta, A.Marked(marked_P), zc ->
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
