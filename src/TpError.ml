module A = Ast
module PP = Pprint

let error = ErrorMsg.error ErrorMsg.Type;;

let error_unknown_var (v,ext) =
  error ext ("unbound variable " ^ v)

let error_unknown_var_right (v,ext) =
  error ext ("unbound variable " ^ v ^ " on the right")

let error_unknown_var_ctx (v,ext) =
  error ext ("unbound variable " ^ v ^ " in the context")

let error_undeclared (f, ext) =
  error ext ("process " ^ f ^ " undeclared");;

let error_implicit (_p, ext) =
  error ext ("not allowed in implicit syntax");;

let error_label_missing_alt (l, ext) =
  error ext ("label " ^ l ^ " does not appear among the alternatives in the type");;

let error_label_invalid env (l, a, c, ext) =
  error ext ("label " ^ l ^ " not a valid alternative in type " ^ PP.pp_tp_compact env a ^ " of channel " ^ c);;

let error_label_mismatch (l, l', ext) =
  error ext ("label " ^ l ^ " is different from " ^ l' ^ "\n"
              ^ "[Hint: the order of branches must follow the order of the alternatives the type]");;

let error_label_missing_branch (l, ext) =
  error ext ("label " ^ l ^ " does not appear among the branches");;

(* structure TpError *)
