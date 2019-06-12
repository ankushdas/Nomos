module R = Arith
module A = Ast
module PS = Parsestate
module M = TokStream
module T = Terminal
module L = Lex
val pp_tok : T.terminal -> string
val pp_toks : T.terminal list -> string
val ( ^^ ) : string -> string -> string
val parse_error : (int * int) * string -> 'a
val msg_expected : T.terminal -> T.terminal -> string
val error_expected : (int * int) * T.terminal * T.terminal -> 'a
val error_expected_h : (int * int) * T.terminal * T.terminal * string -> 'a
val msg_expected_list : T.terminal list -> T.terminal -> string
val error_expected_list : (int * int) * T.terminal list * T.terminal -> 'a
val error_expected_list_h :
  (int * int) * T.terminal list * T.terminal * string -> 'a
val location : ((int * int) * (int * int) * string) option -> string
val vars : ('a * 'b) list -> 'a list
val mark_exp : A.expression * (int * int) -> A.expression
type region = int * int
type prec = int
type stack_item =
    Tok of T.terminal * region
  | ArithInfix of prec * (R.arith * R.arith -> R.arith) * region
  | Arith of R.arith * region
  | Tp of A.stype * region
  | TpInfix of prec * (A.stype * A.stype -> A.stype) * region
  | Context of (A.chan * A.stype) list * region
  | Alts of A.choices
  | Action of (A.expression -> A.expression) * region
  | Args of A.chan list * region
  | Exp of A.expression * region
  | Branches of A.branches
  | Decl of A.decl_ext
type stack = stack_item list
val ( $ ) : 'a list -> 'a -> 'a list
exception StackError
val first : 'a * ('b * 'c) M.front -> 'b
val shift :
  stack_item list * (T.terminal * region) M.front ->
  stack_item list * (T.terminal * region) M.front
val reduce : ('a -> 'b) -> 'a * 'c -> 'b * 'c
val drop : 'a * ('b * 'c) M.front -> 'a * ('b * 'c) M.front
val push : 'a -> 'a list * 'b -> 'a list * 'b
val ( @> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
val ( |> ) : 'a -> ('a -> 'b) -> 'b
val join : 'a * 'b -> 'c * 'd -> 'a * 'd
val here : 'a * ('b * 'c) M.front -> 'c
val nowhere : int * int
val padd : R.arith * R.arith -> R.arith
val psub : R.arith * R.arith -> R.arith
val pmult : R.arith * R.arith -> R.arith
val ptensor : A.stype * A.stype -> A.stype
val plolli : A.stype * A.stype -> A.stype
exception UnknownParseError
exception UnknownReduceDeclError
exception UnknownReduceTypeError
exception UnknownReduceArithError
exception UnknownReduceChoiceError
exception UnknownReducePrecError
exception UnknownReduceContextError
val p_decl :
  stack_item list * (T.terminal * region) M.front ->
  stack_item list * (T.terminal * region) M.front
val p_eq_type :
  stack_item list * (T.terminal * region) M.front ->
  stack_item list * (T.terminal * region) M.front
val p_eqtype :
  stack_item list * (T.terminal * region) M.front ->
  stack_item list * (T.terminal * region) M.front
val p_exp_decl_def :
  stack_item list * (T.terminal * region) M.front ->
  stack_item list * (T.terminal * region) M.front
val p_context_opt :
  stack_item list * (T.terminal * region) M.front ->
  stack_item list * (T.terminal * region) M.front
val p_context :
  stack_item list * (T.terminal * region) M.front ->
  stack_item list * (T.terminal * region) M.front
val p_context2 :
  stack_item list * (T.terminal * region) M.front ->
  stack_item list * (T.terminal * region) M.front
val r_chan_tp : stack_item list -> stack_item list
val p_turnstile_id_tp :
  stack_item list * (T.terminal * region) M.front ->
  stack_item list * (T.terminal * region) M.front
val p_exp_def :
  stack_item list * (T.terminal * region) M.front ->
  stack_item list * (T.terminal * region) M.front
val p_id_tp :
  stack_item list * (T.terminal * region) M.front ->
  stack_item list * (T.terminal * region) M.front
val r_decl : stack_item list -> stack_item list
val p_idx :
  stack_item list * (T.terminal * region) M.front ->
  stack_item list * (T.terminal * region) M.front
val p_arith :
  stack_item list * (T.terminal * region) M.front ->
  stack_item list * (T.terminal * region) M.front
val p_arith_prec :
  stack_item list * (T.terminal * region) M.front ->
  stack_item list * (T.terminal * region) M.front
val r_arith : stack_item list -> stack_item list
val r_idx : stack_item list -> stack_item list
val p_type :
  stack_item list * (T.terminal * region) M.front ->
  stack_item list * (T.terminal * region) M.front
val p_type_prec :
  stack_item list * (T.terminal * region) M.front ->
  stack_item list * (T.terminal * region) M.front
val p_tpopr_ltri :
  stack_item list * (T.terminal * region) M.front ->
  stack_item list * (T.terminal * region) M.front
val p_tpopr_rtri :
  stack_item list * (T.terminal * region) M.front ->
  stack_item list * (T.terminal * region) M.front
val p_type_opt :
  stack_item list * (T.terminal * region) M.front ->
  stack_item list * (T.terminal * region) M.front
val r_type : stack_item list -> stack_item list
val p_choices :
  stack_item list * (T.terminal * region) M.front ->
  stack_item list * (T.terminal * region) M.front
val p_choices1 :
  stack_item list * (T.terminal * region) M.front ->
  stack_item list * (T.terminal * region) M.front
val p_choices2 :
  stack_item list * (T.terminal * region) M.front ->
  stack_item list * (T.terminal * region) M.front
val r_choices : stack_item list -> stack_item list
val m_exp : A.expression * (int * int) -> A.expression
val p_exp :
  stack_item list * (T.terminal * region) M.front ->
  stack_item list * (T.terminal * region) M.front
val r_exp : stack_item list -> stack_item list
val p_fwd_or_spawn_or_label_send_or_chan_recv :
  stack_item list * (T.terminal * region) M.front ->
  stack_item list * (T.terminal * region) M.front
val p_fwd_or_spawn_or_recv :
  stack_item list * (T.terminal * region) M.front ->
  stack_item list * (T.terminal * region) M.front
val p_fwd_or_spawn :
  stack_item list * (T.terminal * region) M.front ->
  stack_item list * (T.terminal * region) M.front
val p_id_list_opt_exp :
  stack_item list * (T.terminal * region) M.front ->
  stack_item list * (T.terminal * region) M.front
val r_arg : stack_item list -> stack_item list
val p_idx_opt :
  stack_item list * (T.terminal * region) M.front ->
  stack_item list * (T.terminal * region) M.front
val r_exp_atomic : stack_item list -> stack_item list
val r_action : stack_item list -> stack_item list
val p_branches :
  stack_item list * (T.terminal * region) M.front ->
  stack_item list * (T.terminal * region) M.front
val p_branches2 :
  stack_item list * (T.terminal * region) M.front ->
  stack_item list * (T.terminal * region) M.front
val r_branch : stack_item list -> stack_item list
val p_id :
  stack_item list * (T.terminal * region) M.front ->
  stack_item list * (T.terminal * region) M.front
val p_terminal :
  T.terminal ->
  stack_item list * (T.terminal * region) M.front ->
  stack_item list * (T.terminal * region) M.front
val p_terminal_h :
  T.terminal ->
  string ->
  stack_item list * (T.terminal * region) M.front ->
  stack_item list * (T.terminal * region) M.front
val parse_decls : (T.terminal * region) M.front -> A.decl_ext list
val parse : string -> A.decl_ext list
val parse_preamble_decls : (T.terminal * region) M.front -> A.decl_ext list
val parse_preamble : string -> A.decl_ext list
