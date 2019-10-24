module A = Ast
module PP = Pprint

let error = ErrorMsg.error ErrorMsg.Type;;

let error_unknown_var ((v,_m),ext) =
  error ext ("unbound variable " ^ v)

let error_unknown_var_right ((v,_m),ext) =
  error ext ("unbound variable " ^ v ^ " on the right")

let error_unknown_var_ctx ((v,_m),ext) =
  error ext ("unbound variable " ^ v ^ " in the context")

let error_undeclared (f, ext) =
  error ext ("process " ^ f ^ " undeclared");;

let error_implicit (_p, ext) =
  error ext ("not allowed in implicit syntax");;

let error_label_missing_alt (l, ext) =
  error ext ("label " ^ l ^ " does not appear among the alternatives in the type");;

let error_label_invalid env (l, a, c, ext) =
  error ext ("label " ^ l ^ " not a valid alternative in type " ^ PP.pp_tp_compact env a ^ " of channel " ^ PP.pp_chan c);;

let error_label_mismatch (l, l', ext) =
  error ext ("label " ^ l ^ " is different from " ^ l' ^ "\n"
              ^ "[Hint: the order of branches must follow the order of the alternatives the type]");;

let error_label_missing_branch (l, ext) =
  error ext ("label " ^ l ^ " does not appear among the branches");;

let error_potstar ext =
  error ext ("found unknown potential * while typechecking");;

let error_mode_mismatch ((v1,m1), (v2,m2), ext) =
  error ext ("mode mismatch: " ^ PP.pp_chan (v1,m1) ^ " != " ^ PP.pp_chan (v2,m2));;

let error_mode_shared_comm ((v,m), ext) =
  error ext ("linear communication on " ^ v ^ " at mode " ^ PP.pp_mode m);;
  
(* structure TpError *)
