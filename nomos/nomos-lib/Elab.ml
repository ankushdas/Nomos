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
module F = NomosFlags
module EM = ErrorMsg
module IO = InOut

let error = ErrorMsg.error ErrorMsg.Type;;

(*********************)
(* Validity of Types *)
(*********************)

let pp_costs () =
  "-work=" ^ F.pp_cost (!F.work);;

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

let check_nonneg pot ext =
  match pot with
      A.Star -> ()
    | A.Arith pot ->
        if R.non_neg pot then ()
        else error ext ("process potential " ^ R.pp_arith pot ^ " not positive");;

(***************************)
(* Elaboration, First Pass *)
(***************************)

(* elab_decl env decls = env'
 * checks that types are contractive and sub-synchronizing.
 * checks that declaration has no duplicates.
 * checks types in declaration are sub-synchronizing.
 * checks potential is non-negative.
 * type checks the process also.
 *)
let rec elab_decl env dcls = match dcls with
    [] -> []
  | ((A.TpDef(v,a), ext') as dcl)::dcls' ->
      let () = if TC.contractive a then ()
               else error ext' ("type " ^ PP.pp_tp env a ^ " not contractive") in
      (*let () = TC.valid env TC.Zero a ext in*)
      let () = TC.ssync_tp env (A.TpName(v)) ext' in
      dcl::(elab_decl env dcls')
  | (A.ExpDecDef(f,m,(delta,pot,(x,a)),p), ext')::dcls' ->
      (* do not print process declaration so they are printed close to their use *)
      let () = if dups ((x,a)::delta.linear) then error ext' ("duplicate variable in process declaration") else () in
      let () = TC.ssync_tp env a ext' in
      (* let _ctx = ssync_ctx env delta ext' in *)
      let () = check_nonneg pot ext' in
      let p' = Cost.apply_cost p in (* applying the cost model *)
      let () = IO.add_cost_exp (A.ExpDecDef(f,m,(delta,pot,(x,a)),p'),ext') in
      let () =
        begin
          match !F.syntax with                    (* print reconstructed term *)
              F.Implicit ->
              (* ask Ankush *)
                EM.error EM.Pragma ext' "implicit syntax currently not supported"
            | F.Explicit -> (* maybe only if there is a cost model... *)
                if !F.verbosity >= 2
                then print_string ("% with cost model " ^ pp_costs () ^ "\n"
                                  ^ (PP.pp_decl env (A.ExpDecDef(f,m,(delta,pot,(x,a)),p'))) ^ "\n")
                else ()
        end
      in
      let () = try TC.checkfexp false env delta pot p' (x,a) ext' m (* approx. type check *)
               with EM.TypeError msg ->
                  (* if verbosity >= 2, type-check again, this time with tracing *)
                  if !F.verbosity >= 2
                    then
                      begin
                        print_string ("% tracing type checking...\n")
                        ; TC.checkfexp true env delta pot p' (x,a) ext' m
                      end (* will re-raise ErrorMsg.Error *)
                  else raise (EM.TypeError msg) (* re-raise if not in verbose mode *) in
      (A.ExpDecDef(f,m,(delta,pot,(x,a)),p'), ext')::(elab_decl env dcls')
  | ((A.Exec(f), ext') as dcl)::dcls' ->
      begin
        match A.lookup_expdec env f with
            Some (ctx,_pot,_zc,_m) ->
              if List.length ctx.A.ordered > 0
              then error ext' ("process " ^ f ^ " has a non-empty context, cannot be executed")
              else dcl::(elab_decl env dcls')
          | None -> error ext' ("process " ^ f ^ " undefined")
      end;;

(* elab_decls env decls = SOME(env')
 * if elaboration of decls succeeds with respect to env, yielding env'
 * Returns NONE if there is a static error
 *)
let elab_decls env dcls =
  (* first pass: check validity of types and type check processes *)
  let env'' = elab_decl env dcls in
  env'';;

exception ElabImpossible;;

(**************************)
(* Checking Redeclaration *)
(**************************)

(* Because of mutual recursion, we cannot redefine or
 * redeclare types or processes
 *)


let rec check_redecl dcls = match dcls with
    [] -> ()
  | (A.TpDef(v,_), ext)::dcls' ->
      if TC.is_tpdef dcls' v
      then error ext ("type name " ^ v ^ " defined more than once")
      else check_redecl dcls'
  | (A.ExpDecDef(f,_,_,_), ext)::dcls' ->
      if TC.is_expdecdef dcls' f
      then error ext ("process name " ^ f ^ " defined more than once")
      else check_redecl dcls'
  | (A.Exec(_), _)::dcls' -> check_redecl dcls';;

let rec get_one_exec dcls cnt g = match dcls with
    [] ->
      if cnt = 1
      then g
      else if cnt = 0 then raise (EM.TypeError "no execs in transaction")
      else raise (EM.TypeError "more than 1 exec in transaction")
  | (A.Exec(f), _ext)::dcls' -> get_one_exec dcls' (cnt+1) f
  | (A.TpDef _, _ext)::dcls' | (A.ExpDecDef _, _ext)::dcls' -> get_one_exec dcls' cnt g;;

let rec check_valid env dcls = match dcls with
    [] -> ()
  | (A.TpDef(_v,a), ext)::dcls' ->
      let () = TC.check_declared env ext a in
      check_valid env dcls'
  | (A.ExpDecDef(_f,_m,(ctx,_pot,(_x,a)),_p), ext)::dcls' ->
      let () = TC.check_declared_ctx env ext ctx in
      let () = TC.check_declared env ext a in
      check_valid env dcls'
  | (A.Exec(f), ext)::dcls' ->
      if TC.is_expdecdef env f
      then check_valid env dcls'
      else error ext ("process " ^ f ^ " undeclared");;

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
        with A.UndeclaredTp ->
          error None ("type " ^ PP.pp_tp_compact env t ^ " undeclared");;

let rec commit_channels env dcls = match dcls with
    [] -> []
  | (A.ExpDecDef(f,m,(delta,pot,(x,a)),p), ext)::dcls' ->
      let delta' = commit env delta.ordered delta.ordered in
      (A.ExpDecDef(f,m,(delta',pot,(x,a)),p), ext)::(commit_channels env dcls')
  | dcl::dcls' -> dcl::(commit_channels env dcls');;

(* Replace stars in potential annotations with variables in types and expressions *)
let rec remove_stars_tps dcls = match dcls with
    [] -> []
  | (A.TpDef(v,a), ext)::dcls' ->
      let a' = I.remove_stars_tp a in
      (A.TpDef(v,a'), ext)::(remove_stars_tps dcls')
  | ((A.ExpDecDef _, _) as dcl)::dcls' -> dcl::(remove_stars_tps dcls')
  | ((A.Exec _, _) as dcl)::dcls' -> dcl::(remove_stars_tps dcls');;

let rec remove_stars_exps dcls = match dcls with
    [] -> []
  | (A.ExpDecDef(f,m,(delta,pot,(z,c)),p), ext)::dcls' ->
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
    (A.ExpDecDef(f,m,(delta',pot',zc'),p'), ext)::(remove_stars_exps dcls')
  | ((A.TpDef _, _) as dcl)::dcls' -> dcl::(remove_stars_exps dcls')
  | ((A.Exec _, _) as dcl)::dcls' -> dcl::(remove_stars_exps dcls');;

let remove_stars env =
  let env = remove_stars_tps env in
  let env = remove_stars_exps env in
  env;;

(* remove U from modes, and substitute them with variables *)
let rec removeU_tps dcls = match dcls with
    [] -> []
  | (A.TpDef(v,a), ext)::dcls' ->
      let a' = I.removeU_tp a in
      (A.TpDef(v,a'), ext)::(removeU_tps dcls')
  | ((A.ExpDecDef _, _) as dcl)::dcls' -> dcl::(removeU_tps dcls')
  | ((A.Exec _, _) as dcl)::dcls' -> dcl::(removeU_tps dcls');;

let rec removeU_exps dcls = match dcls with
    [] -> []
  | (A.ExpDecDef(f,m,(delta,pot,(z,c)),p), ext)::dcls' ->
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
    (A.ExpDecDef(f,m,(delta',pot,zc'),p'), ext)::(removeU_exps dcls')
  | ((A.TpDef _, _) as dcl)::dcls' -> dcl::(removeU_exps dcls')
  | ((A.Exec _, _) as dcl)::dcls' -> dcl::(removeU_exps dcls');;

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

let rec gen_constraints env dcls = match dcls with
    [] -> ()
  | (A.ExpDecDef(f,m,(delta,pot,(x,a)),p),ext)::dcls' ->
      let () = well_formedness env f m delta x ext in
      let () = TC.checkfexp false env delta pot p (x,a) ext m in
      gen_constraints env dcls'
  | (A.TpDef _,_ext)::dcls' -> gen_constraints env dcls'
  | (A.Exec _,_ext)::dcls' -> gen_constraints env dcls';;

let rec substitute dcls psols msols = match dcls with
    [] -> []
  | (A.ExpDecDef(f,m,(delta,pot,(z,c)),p), dext)::dcls' ->
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
      (A.ExpDecDef(f,m,(ctx',pot',zc'),p'), dext)::(substitute dcls' psols msols)
  | (A.TpDef(v,a), dext)::dcls' ->
      let a' = I.substitute_tp a psols msols in
      (A.TpDef(v,a'), dext)::(substitute dcls' psols msols)
  | dcl::dcls' -> dcl::(substitute dcls' psols msols);;


(* structure Elab *)
