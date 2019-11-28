module A = Ast
module PP = Pprint

let error = ErrorMsg.error ErrorMsg.Type;;

let error_unknown_var ((s,v,_m)) ext =
  error ext ("unbound variable " ^ PP.pp_structure s ^ v)

let error_unknown_var_right ((s,v,_m)) ext =
  error ext ("unbound variable " ^ PP.pp_structure s ^ v ^ " on the right")

let error_unknown_var_ctx ((s,v,_m)) ext =
  error ext ("unbound variable " ^ PP.pp_structure s ^ v ^ " in the context")

let error_undeclared (f) ext =
  error ext ("process " ^ f ^ " undeclared");;

let error_implicit (_p) ext =
  error ext ("not allowed in implicit syntax");;

let error_label_missing_alt (l) ext =
  error ext ("label " ^ l ^ " does not appear among the alternatives in the type");;

let error_label_invalid env (l, a, c) ext =
  error ext ("label " ^ l ^ " not a valid alternative in type " ^ PP.pp_tp_compact env a ^ " of channel " ^ PP.pp_chan c);;

let error_label_mismatch (l, l') ext =
  error ext ("label " ^ l ^ " is different from " ^ l' ^ "\n"
              ^ "[Hint: the order of branches must follow the order of the alternatives the type]");;

let error_label_missing_branch (l) ext =
  error ext ("label " ^ l ^ " does not appear among the branches");;

let error_potstar () ext =
  error ext ("found unknown potential * while typechecking");;

let error_mode_mismatch ((s1,v1,m1), (s2,v2,m2)) ext =
  error ext ("mode mismatch: " ^ PP.pp_chan (s1,v1,m1) ^ " != " ^ PP.pp_chan (s2,v2,m2));;

let error_mode_shared_comm ((s,v,m)) ext =
  error ext ("linear communication on " ^ PP.pp_structure s ^ v ^ " at mode " ^ PP.pp_mode m);;
  
(* structure TpError *)
