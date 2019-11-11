(* Elaboration *)

(*
 * Perform validity checks for types, reconstruction, type checking
 * and return an environment with elaborated declarations
 *
 * We use a two-pass algorithm.  In the first pass we check types
 * for validity and also create internal names following every
 * type constructor.  These internal names are used in the type
 * equality algorithm to detect cycles.
 *
 * Because we allow mutual recursion for both types and programs
 * many of the functions take an (unvarying) copy of the complete
 * environment in addition to the list of declarations to be elaborated.
 *)

module R = Arith
module A = Ast
module PP = Pprint
module TC = Typecheck
module E = TpError
module I = Infer

let error = ErrorMsg.error ErrorMsg.Type;;
 
let postponed dcl = match dcl with
    A.Exec _ -> "% "
  | A.TpDef _
  | A.ExpDecDef _ -> "";;
 
(*********************)
(* Validity of Types *)
(*********************)
 
let pp_costs () =
  "-work=" ^ Flags.pp_cost (!Flags.work);;
 
 (* dups vs = true if there is a duplicate variable in vs *)
let rec dups delta = match delta with
  | [] -> false
  | (x,_t)::ctx' -> List.exists (fun (v,_t) -> v = x) ctx' || dups ctx';;

(*
let rec valid_ctx env ctx ext = match ctx with
    [] -> ()
  | (_x,t)::ctx' ->
      let () = TC.valid env TC.Zero t ext in
      let () = TC.esync_tp env t ext in
      valid_ctx env ctx' ext;;

let valid_delta env delta ext = valid_ctx env (delta.A.shared @ delta.A.linear) ext;;
*)

let check_nonneg pot _ext =
  match pot with
      A.Star -> ()
    | A.Arith pot -> 
        if R.non_neg pot then ()
        else error ("process potential " ^ R.pp_arith pot ^ " not positive");;

(***************************)
(* Elaboration, First Pass *)
(***************************)


(* elab_tps env decls = env'
 * elaborates all type definitions in decls to generate env'
 * This checks them for validity and creates internal names.
 * Expressions are passed through unchanged, but process declarations
 * are checked for the validity of the types used.
 *)
let rec elab_tps env dcls ext = match dcls with
    [] -> []
  | (A.TpDef(v,a) as dcl)::dcls' ->
      let () = if !Flags.verbosity >= 1
               then print_string (postponed dcl ^ PP.pp_decl env dcl ^ "\n")
               else () in
      let () = if TC.contractive a then ()
               else error ("type " ^ PP.pp_tp env a ^ " not contractive") in
      (*let () = TC.valid env TC.Zero a ext in*)
      let () = TC.esync_tp env (A.TpName(v)) ext in
      dcl::(elab_tps env dcls' ext)
  | (A.ExpDecDef(_f,_m,(delta,pot,(x,a)),_p) as dcl)::dcls' ->
      (* do not print process declaration so they are printed close to their use *)
      let () = if dups ((x,a)::delta.linear) then error ("duplicate variable in process declaration") else () in
      (*let () = valid_delta env delta ext in*)
      (*let () = TC.valid env TC.Zero a ext in*)
      let () = TC.esync_tp env a ext in
      let () = check_nonneg pot ext in
      dcl::(elab_tps env dcls' ext)
  | dcl::dcls' -> dcl::(elab_tps env dcls' ext);;

(****************************)
(* Elaboration, Second Pass *)
(****************************)

exception ElabImpossible;;
 
(* elab_env env decls = env' if all declarations in decls
 * are well-typed with respect to env, elaborating to env'
 * raises exception otherwise.
 * Assumes that types have already been elaborated in the first pass
 *)
let rec elab_exps' env dcls ext = match dcls with
    [] -> []
  | (A.TpDef _)::_ -> (* do not print type definition again *)
      elab_exps env dcls ext
  | dcl::_ ->
      if !Flags.verbosity >= 1
      then print_string (postponed dcl ^ PP.pp_decl env dcl ^ "\n")
      else () ;
      elab_exps env dcls ext
and elab_exps env dcls ext = match dcls with
    [] -> []
  | (A.TpDef _ as dcl)::dcls' ->
    (* already checked validity during first pass *)
      dcl::(elab_exps' env dcls' ext)
  | (A.ExpDecDef(f,m,(delta,pot,(x,a)),p))::dcls' ->
      let () = print_string ("type checking: " ^ f ^ "\n") in
      let p' = Cost.apply_cost p in (* applying the cost model *)
      let () =
        begin
          match !Flags.syntax with                    (* print reconstructed term *)
              Flags.Implicit ->
                ErrorMsg.error ErrorMsg.Pragma ("implicit syntax currently not supported")
            | Flags.Explicit -> (* maybe only if there is a cost model... *)
                if !Flags.verbosity >= 2
                then print_string ("% with cost model " ^ pp_costs () ^ "\n"
                                  ^ (PP.pp_decl env (A.ExpDecDef(f,m,(delta,pot,(x,a)),p'))) ^ "\n")
                else ()
        end
      in
      let () = try TC.checkfexp false env delta pot p' (x,a) ext m (* approx. type check *)
               with ErrorMsg.Error ->
                  (* if verbosity >= 2, type-check again, this time with tracing *)
                  if !Flags.verbosity >= 2
                    then
                      begin
                        print_string ("% tracing type checking...\n")
                        ; TC.checkfexp true env delta pot p' (x,a) ext m
                      end (* will re-raise ErrorMsg.Error *)
                  else raise ErrorMsg.Error (* re-raise if not in verbose mode *) in
      A.ExpDecDef(f,m,(delta,pot,(x,a)),p')::(elab_exps' env dcls' ext)
  | (A.Exec(f) as dcl)::dcls' ->
      begin
        match A.lookup_expdec env f with
            Some (ctx,_pot,_zc,_m) ->
              if List.length ctx.A.ordered > 0
              then error ("process " ^ f ^ " has a non-empty context, cannot be executed")
              else dcl::elab_exps' env dcls' ext
          | None -> error ("process " ^ f ^ " undefined")
      end;;

(* elab_decls env decls = SOME(env')
 * if elaboration of decls succeeds with respect to env, yielding env'
 * Returns NONE if there is a static error
 *)
let elab_decls env dcls ext =
  (* first pass: check validity of types and create internal names *)
  try
  let env' = elab_tps env dcls ext in
  (* second pass: perform reconstruction and type checking *)
  let env'' = elab_exps' env' env' ext in
  Some env''

  with ErrorMsg.Error -> None;;
 
 (**************************)
 (* Checking Redeclaration *)
 (**************************)
 
 (* Because of mutual recursion, we cannot redefine or
  * redeclare types or processes
  *)
 
 let is_tpdef env a = match A.lookup_tp env a with None -> false | Some _ -> true;;
 let is_expdecdef env f = match A.lookup_expdef env f with None -> false | Some _ -> true;;
 
let rec check_redecl env dcls = match dcls with
    [] -> ()
  | A.TpDef(v,_)::dcls' ->
      if is_tpdef env v || is_tpdef dcls' v
      then error ("type name " ^ v ^ " defined more than once")
      else check_redecl env dcls'
  | A.ExpDecDef(f,_,_,_)::dcls' ->
      if is_expdecdef env f || is_expdecdef dcls' f
      then error ("process name " ^ f ^ " defined more than once")
      else check_redecl env dcls'
  | (A.Exec _)::dcls' -> check_redecl env dcls';;

(* separates channels into shared and linear *)

let rec commit env ctx octx = match ctx with
    [] -> {A.shared = [] ; A.linear = [] ; A.ordered = octx}
  | A.Functional _::ctx' -> commit env ctx' octx
  | A.STyped (c,t)::ctx' ->
      let {A.shared = s; A.linear = l; A.ordered = o} = commit env ctx' octx in
        try
          if A.is_shared env t
          then {A.shared = (c,t)::s ; A.linear = l ; A.ordered = o}
          else {A.shared = s ; A.linear = (c,t)::l ; A.ordered = o}
        with A.UndeclaredTp -> error ("type " ^ PP.pp_tp_compact env t ^ " undeclared");;

let rec commit_channels env dcls = match dcls with
    [] -> []
  | A.ExpDecDef(f,m,(delta,pot,(x,a)),p)::dcls' ->
      let delta' = commit env delta.ordered delta.ordered in
      A.ExpDecDef(f,m,(delta',pot,(x,a)),p)::(commit_channels env dcls')
  | dcl::dcls' -> dcl::(commit_channels env dcls');;

(* Replace stars in potential annotations with variables in types and expressions *)
let rec remove_stars_tps dcls = match dcls with
    [] -> []
  | A.TpDef(v,a)::dcls' ->
      let a' = I.remove_stars_tp a in
      A.TpDef(v,a')::(remove_stars_tps dcls')
  | (A.ExpDecDef _ as dcl)::dcls' -> dcl::(remove_stars_tps dcls')
  | (A.Exec _ as dcl)::dcls' -> dcl::(remove_stars_tps dcls');;

let rec remove_stars_exps dcls = match dcls with
    [] -> []
  | A.ExpDecDef(f,m,(delta,pot,(z,c)),p)::dcls' ->
    let remove_list = List.map (fun (x,a) -> (x, I.remove_stars_tp a)) in
    let remove_arglist = List.map (fun x -> match x with
                                                A.Functional(v,t) -> A.Functional(v,I.remove_stars_ftp t)
                                              | A.STyped(x,a) -> A.STyped(x, I.remove_stars_tp a)) in
    let {A.shared = sdelta; A.linear = ldelta; A.ordered = odelta} = delta in
    let sdelta' = remove_list sdelta in
    let ldelta' = remove_list ldelta in
    let odelta' = remove_arglist odelta in
    let delta' = {A.shared = sdelta'; A.linear = ldelta'; A.ordered = odelta'} in
    let pot' = I.remove_star pot in
    let zc' = (z, I.remove_stars_tp c) in
    let p' = I.remove_stars_faug p in
    A.ExpDecDef(f,m,(delta',pot',zc'),p')::(remove_stars_exps dcls')
  | (A.TpDef _ as dcl)::dcls' -> dcl::(remove_stars_exps dcls')
  | (A.Exec _ as dcl)::dcls' -> dcl::(remove_stars_exps dcls');;

let remove_stars env =
  let env = remove_stars_tps env in
  let env = remove_stars_exps env in
  env;;

(* remove U from modes, and substitute them with variables *)
let rec removeU_tps dcls = match dcls with
    [] -> []
  | A.TpDef(v,a)::dcls' ->
      let a' = I.removeU_tp a in
      A.TpDef(v,a')::(removeU_tps dcls')
  | (A.ExpDecDef _ as dcl)::dcls' -> dcl::(removeU_tps dcls')
  | (A.Exec _ as dcl)::dcls' -> dcl::(removeU_tps dcls');;

let rec removeU_exps dcls = match dcls with
    [] -> []
  | A.ExpDecDef(f,m,(delta,pot,(z,c)),p)::dcls' ->
    let removeU_list = List.map (fun (x,a) -> (I.removeU x, I.removeU_tp a)) in
    let removeU_arglist = List.map (fun x -> match x with
                                                A.Functional(v,t) -> A.Functional(v,t)
                                              | A.STyped (x,a) -> A.STyped (I.removeU x, I.removeU_tp a)) in
    let {A.shared = sdelta; A.linear = ldelta; A.ordered = odelta} = delta in
    let sdelta' = removeU_list sdelta in
    let ldelta' = removeU_list ldelta in
    let odelta' = removeU_arglist odelta in
    let delta' = {A.shared = sdelta'; A.linear = ldelta'; A.ordered = odelta'} in
    let zc' = (I.removeU z, I.removeU_tp c) in
    let p' = I.removeU_faug p in
    A.ExpDecDef(f,m,(delta',pot,zc'),p')::(removeU_exps dcls')
  | (A.TpDef _ as dcl)::dcls' -> dcl::(removeU_exps dcls')
  | (A.Exec _ as dcl)::dcls' -> dcl::(removeU_exps dcls');;

let removeU env =
  let env = removeU_tps env in
  let env = removeU_exps env in
  env;;

let well_formedness env f m delta x ext = match m with
    A.Pure -> TC.pure env f delta x ext
  | A.Shared -> TC.shared env f delta x ext
  | A.Transaction -> TC.transaction env f delta x ext
  | A.Linear
  | A.Unknown
  | A.MVar _ -> raise ElabImpossible

let rec gen_constraints env dcls ext = match dcls with
    [] -> ()
  | A.ExpDecDef(f,m,(delta,pot,(x,a)),p)::dcls' ->
      let () = well_formedness env f m delta x ext in
      let () = TC.checkfexp false env delta pot p (x,a) ext m in
      gen_constraints env dcls' ext
  | A.TpDef _::dcls' -> gen_constraints env dcls' ext
  | A.Exec _::dcls' -> gen_constraints env dcls' ext;;

let rec substitute dcls psols msols = match dcls with
    [] -> []
  | A.ExpDecDef(f,m,(delta,pot,(z,c)),p)::dcls' ->
      let subst_list = List.map (fun (x,a) -> (I.substitute_mode x msols, I.substitute_tp a psols msols)) in
      let subst_arglist = List.map (fun x -> match x with
                                                A.Functional(v,t) -> A.Functional(v, I.substitute_ftp t psols msols)
                                              | A.STyped(x,a) -> A.STyped (I.substitute_mode x msols, I.substitute_tp a psols msols)) in
      let {A.shared = sdelta; A.linear = ldelta; A.ordered = odelta} = delta in
      let sdelta' = subst_list sdelta in
      let ldelta' = subst_list ldelta in
      let odelta' = subst_arglist odelta in
      let ctx' = {A.shared = sdelta'; A.linear = ldelta'; A.ordered = odelta'} in
      let pot' = I.substitute_pot pot psols in
      let zc' = (I.substitute_mode z msols, I.substitute_tp c psols msols) in
      let p' = I.substitute_faug p psols msols in
      A.ExpDecDef(f,m,(ctx',pot',zc'),p')::(substitute dcls' psols msols)
  | A.TpDef(v,a)::dcls' ->
      let a' = I.substitute_tp a psols msols in
      A.TpDef(v,a')::(substitute dcls' psols msols)
  | dcl::dcls' -> dcl::(substitute dcls' psols msols);;


(* structure Elab *)
 