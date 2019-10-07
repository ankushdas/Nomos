(* Parser *)
(* Handwritten shift/reduce parser to support
 * best possible error messages
 *)

module R = Arith
module A = Ast
module PS = Parsestate
module M = TokStream
module T = Terminal
module L = Lex

(******************)
(* Error Messages *)
(******************)
              
let pp_tok t = "'" ^ T.toString t ^ "'";;

let rec pp_toks toks = match toks with
    [] -> ""
  | [t] -> " or " ^ pp_tok t
  | t::ts -> pp_tok t ^ ", " ^ pp_toks ts;;

let parse_error (region, s) =
  ErrorMsg.error_msg ErrorMsg.Parse (PS.ext region) s
  ; raise ErrorMsg.Error;;

let msg_expected t' t =
    ("expected " ^ pp_tok t' ^ ", found: " ^ pp_tok t);;

let error_expected (region, t', t) =
  ErrorMsg.error_msg ErrorMsg.Parse (PS.ext region) (msg_expected t' t)
  ; raise ErrorMsg.Error;;

let msg_expected_list ts t =
    "expected one of " ^ pp_toks ts ^ ", found: " ^ pp_tok t;;

let error_expected_list (region, ts, t) =
  ErrorMsg.error_msg ErrorMsg.Parse (PS.ext region) (msg_expected_list ts t)
  ; raise ErrorMsg.Error;;

(****************************)
(* Building abstract syntax *)
(****************************)

let mark_exp (exp, (left, right)) = A.Marked (Mark.mark' (exp, PS.ext (left, right)));;

(*******************)
(* Data structures *)
(*******************)

type region = int * int;;

(* operator precedence, for arithmetic *)
type prec = int

(* stack items for shift/reduce parsing *)
type stack_item =
    Tok of T.terminal * region   (* lexer token *)
  | ArithInfix of prec * (R.arith * R.arith -> R.arith) * region   (* arithmetic infix operator and constructor *)
  | Arith of R.arith * region                                      (* arithmetic expression *)
  | Star of region                                                 (* star potential *)
  | Tp of A.stype * region                                         (* types *)
  | TpInfix of prec * (A.stype * A.stype -> A.stype) * region      (* infix tensor and lolli type operators *)
  | Context of (A.chan * A.stype) list * region                    (* context *)
  | Alts of A.choices                                              (* list of alternatives in types *)
  | Action of (A.expression -> A.expression) * region              (* prefix process action *)
  | Args of (A.chan list) * region                                 (* arguments for spawn *)
  | Exp of A.expression * region                                   (* process expression *)
  | Branches of A.branches                                         (* list of branches *)
  | Decl of A.decl_ext;;

type stack = stack_item list;;

let ($) s si = si :: s;;

(* This is a hand-written shift/reduce parser
 * 
 * Parsing functions are named p_<nonterminal>, possibly with a suffix
 * for intermediate parsing states.  Reducing functions are named
 * r_<nonterminal>, possibly with a suffix for intermediate states.
 * With few exceptions, a parsing functions have type
 *
 * p_<nonterminal> : stack * L.lexresult M.Front -> stack * L.lexresult M.Front
 * r_<nonterminal> : stack -> stack
 *
 * Note that in input and output of the parsing function, the first
 * token of the lexer stream is exposed (type L.lexresult M.Front) which
 * make for easy matching and composition of these functions.
 *
 * Generally p_<nt> will consume terminals for an <nt> from the lexresult
 * stream and return with the resulting abstract syntax for <nt> on the stack.
 * Generally r_<nt> will consume a mix of terminal and nonterminals on
 * the stack and push the abstract syntax for an <nt> onto the stack.
 *
 * While parsing expression with infix, prefix, and postfix operators
 * we continue parsing, extending the expression until we encounter
 * a terminal that completes that is not part of an expression.
 *
 * p_<nt> is a function that parses nonterminal <nt>
 * r_<nt> is a function that reduces nonterminal <nt>
 * c_<cond> is a function that checks condition <cond>
 * e_<error> is a function that reports error <error>
 * m_<nt> is a function that marks nonterminal <nt> with region information
 *)

(***********************)
(* Parsing Combinators *)
(***********************)

exception StackError

(* Always call 'first ST' to extract the first token for examination *)
let first st = match st with
    (_s, M.Cons((t, _r), _ts')) -> t
  | _t -> raise StackError;;

let shift st = match st with
    (s, M.Cons((t, r), ts')) -> (s $ Tok(t, r), M.force ts')
  | _t -> raise StackError;;

let reduce reduce_fun (s, ft) = (reduce_fun s, ft);;

let drop st = match st with
    (s, M.Cons((_t, _r), ts')) -> (s, M.force ts')   (* use sparingly *)
  | _t -> raise StackError;;

let push item (s, ft) = (s $ item, ft);;

let (@>) f g = fun x -> g(f(x));;
let (|>) x f = f x;;

(* region manipulation *)
let join (left1, _right1) (_left2, right2) = (left1, right2);;
let here st = match st with
    (_s, M.Cons((_t, r), _ts')) -> r
  | _t -> raise StackError;;

let padd (x,y) = R.Add(x,y);;
let psub (x,y) = R.Sub(x,y);;
let pmult (x,y) = R.Mult(x,y);;

let ptensor (s,t) = A.Tensor(s,t);;
let plolli (s,t) = A.Lolli(s,t);;

(***********)
(* Parsing *)
(***********)

(*
 * Refer to the grammar in readme.txt
 * Comments refer to the nonterminals shown there in <angle brackets>
 *)

exception UnknownParseError
exception UnknownReduceDeclError
exception UnknownReduceTypeError
exception UnknownReduceArithError
exception UnknownReduceChoiceError
exception UnknownReducePrecError
exception UnknownReduceContextError

let uncommit ctx =
  {A.shared = [] ; A.linear = [] ; A.ordered = ctx};;

(* <decl> *)
let rec p_decl st = match first st with
    T.TYPE -> st |> shift @> p_id @> p_eq_type @> reduce r_decl
  | T.EQTYPE -> st |> shift @> p_eqtype @> reduce r_decl
  | T.PROC -> st |> shift @> p_id @> p_exp_decl_def @> reduce r_decl
  | T.EXEC -> st |> shift @> p_id @> reduce r_decl
  | T.PRAGMA _ -> st |> shift @> reduce r_decl
  | T.EOF -> st
  | t -> parse_error (here st, "unexpected token " ^ pp_tok t ^ " at top level")

(* '=' <type> *)
and p_eq_type st = match first st with
    T.EQ -> st |> shift @> p_type
  | t -> error_expected (here st, T.EQ, t)

(* <id> '=' <id> *)
and p_eqtype st = st |> p_id @> p_terminal T.EQ @> p_id

(* ':' <context_opt> <turnstile> <id> ':' <type> '=' <exp> *)
and p_exp_decl_def st = match first st with
    T.COLON -> st |> shift @> p_context_opt @> p_turnstile_id_tp @> p_exp_def
  | t -> error_expected (here st, T.COLON, t)

and p_context_opt st = match first st with
    T.PERIOD -> st |> drop @> push (Context([], here st))
  | T.LPAREN -> st |> push (Context([], here st)) @> p_context
  | t -> error_expected_list (here st, [T.PERIOD; T.LPAREN], t)
  
and p_context st = st |> p_terminal T.LPAREN @> p_id @> p_terminal T.COLON @> p_type @> p_terminal T.RPAREN @> reduce r_chan_tp @> p_context2

and p_context2 st = match first st with
    T.COMMA -> st |> drop @> p_context
  | T.TURNSTILE | T.BAR -> st
  | t -> error_expected_list (here st, [T.COMMA; T.TURNSTILE; T.BAR], t)

and r_chan_tp st = match st with
    Tok(T.RPAREN, r2) :: Tp(tp,_r2) :: Tok(T.COLON,_) :: Tok(T.IDENT(id), _r1) :: Tok(T.LPAREN, _) :: Context(ctx, r) :: s -> s $ Context(ctx @ [(id,tp)], join r r2)
  | _st -> raise UnknownReduceContextError

(* <turnstile> <id> : <type> *)
and p_turnstile_id_tp st = match first st with
    T.TURNSTILE -> st |> shift @> p_id_tp
  | T.BAR -> st |> shift @> p_idx_or_star @> p_terminal T.MINUS @> p_id_tp
  | t -> error_expected_list (here st, [T.TURNSTILE; T.BAR], t)

and p_exp_def st = match first st with
    T.EQ -> st |> shift @> p_exp
  | t -> error_expected (here st, T.EQ, t)

and p_id_tp st = st |> p_terminal T.LPAREN @> p_id @> p_terminal T.COLON @> p_type @> p_terminal T.RPAREN

(* reduce top-level declaration *)
and r_decl st_decl = match st_decl with
    (* 'type' <id> = <type> *)
    Tp(tp,r2) :: Tok(T.EQ,_) :: Tok(T.IDENT(id),_) :: Tok(T.TYPE,r1) :: s ->
    s $ Decl({A.declaration = A.TpDef(id,tp); decl_extent = PS.ext(join r1 r2)})

    (* 'eqtype' <id> = <id> *)
  |  Tok(T.IDENT(id2),r2) :: Tok(T.EQ,_r) :: Tok(T.IDENT(id1),_r1) :: Tok(T.EQTYPE,r1) :: s ->
    s $ Decl({declaration = A.TpEq(A.TpName(id1),A.TpName(id2)); decl_extent = PS.ext(join r1 r2)})

    (* 'proc' <id> : <context> |- <id> : <type> = <exp> *)
  | Exp(p,r2) :: Tok(T.EQ,_) :: Tok(T.RPAREN,_) :: Tp(tp,_) :: Tok(T.COLON,_) :: Tok(T.IDENT(c),_) :: Tok(T.LPAREN,_) ::
    Tok(T.TURNSTILE,_) :: Context(ctx,_) :: Tok(T.COLON,_) ::
    Tok(T.IDENT(id),_) :: Tok(T.PROC,r1) :: s ->
    s $ Decl({A.declaration = A.ExpDecDef(id,(uncommit ctx,A.Arith (R.Int 0),(c,tp)),p); decl_extent = PS.ext(join r1 r2)})
  
    (* 'proc' <id> : <context> '|{' <arith> '}-' <id> : <type> = <exp> *)
  | Exp(p,r2) :: Tok(T.EQ,_) :: Tok(T.RPAREN,_) :: Tp(tp,_) :: Tok(T.COLON,_) :: Tok(T.IDENT(c),_) :: Tok(T.LPAREN,_) ::
    Tok(T.MINUS,_) :: Arith(pot,_) :: Tok(T.BAR,_) :: Context(ctx,_) :: Tok(T.COLON,_) ::
    Tok(T.IDENT(id),_) :: Tok(T.PROC,r1) :: s ->
    s $ Decl({A.declaration = A.ExpDecDef(id,(uncommit ctx,A.Arith pot,(c,tp)),p); decl_extent = PS.ext(join r1 r2)})

    (* 'exec' <id> *)
  | Tok(T.IDENT(f),r2) :: Tok(T.EXEC,r1) :: s ->
    s $ Decl({A.declaration = A.Exec(f); decl_extent = PS.ext(join r1 r2)})

    (* '#' <line> '\n' *)
  | Tok(T.PRAGMA(p,line),r) :: s ->
    s $ Decl({A.declaration = A.Pragma(p,line); decl_extent = PS.ext(r)})
  
    (* should be the only possibilities *)
  | _s -> raise UnknownReduceDeclError

(* <idx> ::= '{' <arith> '}' or '{*}' *)
and p_idx_or_star st = match first st with
    T.LBRACE -> st |> shift @> p_arith_or_star @> p_terminal T.RBRACE @> reduce r_idx_or_star
  | t -> error_expected (here st, T.LBRACE, t)

and p_arith_or_star st = match first st with
    T.STAR -> st |> shift
  | _t -> st |> p_arith

(* <arith> *)
and p_arith st = match first st with
    T.NAT(_n) -> st |> shift @> reduce r_arith @> p_arith
  | T.LPAREN -> st |> shift @> p_arith @> p_terminal T.RPAREN @> reduce r_arith @> p_arith
  | T.PLUS -> st |> drop @> push (ArithInfix(1, padd, here st)) @> p_arith_prec
  | T.MINUS -> st |> drop @> push (ArithInfix(1, psub, here st)) @> p_arith_prec
  | T.STAR -> st |> drop @> push (ArithInfix(2, pmult, here st)) @> p_arith_prec
  | _t -> st |> reduce r_arith

(* <arith> *)
(* shift/reduce decision based on operator precedence *)
and p_arith_prec st = match st with
    (ArithInfix(prec, con, r) :: Arith(e2, r2) :: ArithInfix(prec1, con1, _) :: Arith(e1, r1) :: s, ft) ->
      if prec1 >= prec (* all operators are left associative *)
      then p_arith_prec (s $ Arith(con1(e1,e2), join r1 r2) $ ArithInfix(prec, con, r), ft) (* reduce *)
      else p_arith st          (* shift *)
  | (ArithInfix(_prec, _con, _) ::Arith(_e,_r) ::  _s, _ft) -> p_arith st (* shift *)
  | (ArithInfix(_,_,r2) :: ArithInfix(_,_,r1) :: _s, _ft) -> parse_error (join r1 r2, "consecutive infix operators")
  | (ArithInfix(_,_,r) :: _s, _ft) -> parse_error (r, "leading infix operator")
  | _st -> raise UnknownReducePrecError

(* reduce <arith> *)
and r_arith st = match st with
    Tok(T.NAT(n),r) :: s -> s $ Arith(R.Int(n), r)
  | Tok(T.RPAREN, r2) :: Arith(e, _) :: Tok(T.LPAREN, r1) :: s -> s $ Arith(e, join r1 r2)
  | Arith(e2, r2) :: ArithInfix(_, con, _) :: Arith(e1, r1) :: s -> r_arith (s $ Arith(con(e1,e2), join r1 r2))
  | Arith(_e2, r2) :: Arith(_e1, r1) :: _s -> parse_error (join r1 r2, "consecutive arithmetic expressions")
  | ArithInfix(_, _, r) :: _s -> parse_error (r, "trailing infix operator")
  | Arith(e,r) :: s -> s $ Arith(e,r)
  | Tok(_,r) :: _s -> parse_error (r, "empty arithmetic expression")
  | _st -> raise UnknownReduceArithError
  (* arithmetic expressions are always preceded by '{' or '(' *)

(* reduce '{' <arith> '}' *)
and r_idx_or_star st = match st with
    Tok(T.RBRACE,r2) :: Arith(e, _) :: Tok(T.LBRACE,r1) :: s -> s $ Arith (e, join r1 r2)
  | Tok(T.RBRACE,r2) :: Tok(T.STAR,_) :: Tok(T.LBRACE,r1) :: s -> s $ Star (join r1 r2)
  | _t -> raise UnknownReduceArithError

(* <tp> *)
and p_type st = match first st with
    T.NAT(1) -> st |> shift @> reduce r_type @> p_type
  | T.NAT(n) -> parse_error (here st, "expected type, found " ^ string_of_int n)
  | T.PLUS -> st |> shift @> p_choices @> reduce r_type @> p_type
  | T.AMPERSAND -> st |> shift @> p_choices @> reduce r_type @> p_type
  | T.LPAREN -> st |> shift @> p_type @> p_terminal T.RPAREN @> reduce r_type @> p_type
  | T.LANGLE -> st |> shift @> p_tpopr_ltri @> p_type @> reduce r_type @> p_type (* maybe not shift *)
  | T.BAR -> st |> shift @> p_tpopr_rtri @> p_type @> reduce r_type @> p_type    (* maybe not shift *)
  | T.STAR -> st |> drop @> push (TpInfix(1, ptensor, here st)) @> p_type_prec
  | T.LOLLI -> st |> drop @> push (TpInfix(1, plolli, here st)) @> p_type_prec
  | T.UP -> st |> shift @> p_type @> reduce r_type @> p_type
  | T.DOWN -> st |> shift @> p_type @> reduce r_type @> p_type
  | T.IDENT(_id) -> st |> p_id @> reduce r_type @> p_type
  | _t -> st |> reduce r_type
    
(* <arith> *)
(* shift/reduce decision based on operator precedence *)
and p_type_prec st = match st with
    (TpInfix(prec, con, r) :: Tp(tp2, r2) :: TpInfix(prec1, con1, _) :: Tp(tp1, r1) :: s, ft) ->
      if prec1 > prec (* all type operators are right associative *)
      then p_type_prec (s $ Tp(con1(tp1,tp2), join r1 r2) $ TpInfix(prec, con, r), ft) (* reduce *)
      else p_type st          (* shift *)
  | (TpInfix(_prec, _con, _) :: Tp(_,_) :: _s, _ft) -> p_type st (* shift *)
  | (TpInfix(_,_,r2) :: TpInfix(_,_,r1) :: _s, _ft) -> parse_error (join r1 r2, "consecutive infix type operators")
  | (TpInfix(_,_,r) :: _s, _ft) -> parse_error (r, "leading infix type operator")
  | _st -> raise UnknownReducePrecError


(* '>' | '|' | <idx> '|' *)
(* follows '<' to parse diamond or left triangle *)
and p_tpopr_ltri st = match first st with
  | T.BAR -> st |> shift
  | T.LBRACE -> st |> p_idx_or_star @> p_terminal T.BAR
  | t -> error_expected_list (here st, [T.BAR; T.LBRACE], t)

(* '>' | <idx> '>' *)
(* follows '|' to parse right triangle *)
and p_tpopr_rtri st = match first st with
  | T.RANGLE -> st |> shift
  | T.LBRACE -> st |> p_idx_or_star @> p_terminal T.RANGLE
  | t -> error_expected_list (here st, [T.RANGLE; T.LBRACE], t)                           

(* reduce <type> *)
and r_type st = match st with
  | Tok(T.NAT(1),r) :: s -> s $ Tp(A.One, r)
  | Tok(T.RBRACE,r2) :: Alts(alts) :: Tok(T.LBRACE,_) :: Tok(T.PLUS,r1) :: s ->
    s $ Tp(A.Plus(alts), join r1 r2)
  | Tok(T.RBRACE,r2) :: Alts(alts) :: Tok(T.LBRACE,_) :: Tok(T.AMPERSAND,r1) :: s ->
    s $ Tp(A.With(alts), join r1 r2)
  | Tp(tp, r2) :: Tok(T.BAR,_) :: Tok(T.LANGLE,r1) :: s  -> s $ Tp(A.GetPot (A.Arith (R.Int 1),tp), join r1 r2)
  | Tp(tp, r2) :: Tok(T.BAR,_) :: Arith(p,_) :: Tok(T.LANGLE,r1) :: s -> s $ Tp(A.GetPot(A.Arith p,tp), join r1 r2)
  | Tp(tp, r2) :: Tok(T.BAR,_) :: Star(_) :: Tok(T.LANGLE,r1) :: s -> s $ Tp(A.GetPot(A.Star,tp), join r1 r2)
  | Tp(tp, r2) :: Tok(T.RANGLE,_) :: Tok(T.BAR,r1) :: s -> s $ Tp(A.PayPot (Arith (R.Int 1),tp), join r1 r2)
  | Tp(tp, r2) :: Tok(T.RANGLE,_) :: Arith(p,_) :: Tok(T.BAR,r1) :: s -> s $ Tp(A.PayPot(A.Arith p,tp), join r1 r2)
  | Tp(tp, r2) :: Tok(T.RANGLE,_) :: Star(_) :: Tok(T.BAR,r1) :: s -> s $ Tp(A.PayPot(A.Star,tp), join r1 r2)
  | Tp(tp, r2) :: Tok(T.UP,r1) :: s -> s $ Tp(A.Up(tp), join r1 r2)
  | Tp(tp, r2) :: Tok(T.DOWN,r1) :: s -> s $ Tp(A.Down(tp), join r1 r2)
  | Tok(T.IDENT(id),r) :: s -> s $ Tp(A.TpName(id),r)
  | Tok(T.RPAREN, r2) :: Tp(tp, _) :: Tok(T.LPAREN, r1) :: s -> s $ Tp(tp, join r1 r2)
  | Tp(tp2, r2) :: TpInfix(_, con, _) :: Tp(tp1, r1) :: s -> r_type (s $ Tp(con(tp1,tp2), join r1 r2))
  | Tp(_tp2, r2) :: Tp(_tp1, r1) :: _s -> parse_error (join r1 r2, "consecutive types")
  | TpInfix(_,_,r) :: _s -> parse_error (r, "trailing infix type operator")
  | Tp(tp,r) :: s -> s $ Tp(tp,r)
  | Tok(_,r) :: _s -> parse_error (r, "unknown or empty type expression")
  (* should be the only possibilities *)
  | _st -> raise UnknownReduceTypeError

(* <choices> *)
(* Alts (_) accumulates alternative choices *)
and p_choices st =
    st |> p_terminal T.LBRACE @> push (Alts([])) @> p_choices1 @> p_terminal T.RBRACE

and p_choices1 st = st |> p_id @> p_terminal T.COLON @> p_type @> reduce r_choices @> p_choices2

and p_choices2 st = match first st with
    T.RBRACE -> st (* do not shift reduce *)
  | T.COMMA -> st |> drop @> p_choices1
  | t -> error_expected_list (here st, [T.RBRACE; T.COMMA], t)

(* reduce <choices> *)
and r_choices st = match st with
    Tp(tp,_r2) :: Tok(T.COLON,_) :: Tok(T.IDENT(id),_r1) :: Alts(alts) :: s -> s $ Alts(alts @ [(id,tp)])
    (* should be the only possibility *)
  | _st -> raise UnknownReduceChoiceError

and m_exp (exp, r) = mark_exp (exp, r)

(* <exp> *)
and p_exp st = match first st with
    T.IDENT(_id) -> st |> shift @> p_fwd_or_spawn_or_label_send_or_chan_recv_or_shared
  | T.CASE -> st |> shift @> p_id @> p_terminal T.LPAREN @> push (Branches []) @> p_branches @> p_terminal T.RPAREN @> reduce r_exp_atomic @> p_exp
  | T.CLOSE -> st |> shift @> p_id @> reduce r_exp_atomic @> p_exp
  | T.WAIT -> st |> shift @> p_id @> p_terminal T.SEMICOLON @> reduce r_action @> p_exp
  | T.SEND -> st |> shift @> p_id @> p_id @> p_terminal T.SEMICOLON @> reduce r_action @> p_exp
  | T.LPAREN -> st |> shift @> p_exp @> p_terminal T.RPAREN @> reduce r_exp_atomic @> p_exp
  (* expressions for work analysis *)
  | T.WORK -> st |> shift @> p_idx_opt @> p_exp
  | T.PAY -> st |> shift @> p_id @> p_idx_opt @> p_exp
  | T.GET -> st |> shift @> p_id @> p_idx_opt @> p_exp
  (* end of expression; do not consume token *)
  | _t -> st |> reduce r_exp

(* reduce <exp>, possibly multiple actions, cuts, or expressions *)
(* stack ends with Action, Cut, or Exp items *)
and r_exp st = match st with
    Exp(exp,r2) :: Action(act,r1) :: s ->
      r_exp (s $ Exp(act(exp), join r1 r2))
  | Action(_act, r) :: _s -> parse_error (r, "incomplete action")
  | Exp(_exp2,r2) :: Exp(_exp1,r1) :: _s -> parse_error (join r1 r2, "consecutive expressions")
  (* done reducing *)
  | Exp(exp,r) :: s -> s $ Exp(exp,r)
  | Tok(t,r) :: _s -> parse_error (r, "unknown or empty expression: " ^ pp_tok t)
  | _t -> raise UnknownParseError

and p_fwd_or_spawn_or_label_send_or_chan_recv_or_shared st = match first st with
    T.PERIOD -> st |> shift @> p_id @> p_terminal T.SEMICOLON @> reduce r_action @> p_exp
  | T.LARROW -> st |> shift @> p_fwd_or_spawn_or_recv_or_shared
  | t -> error_expected_list (here st, [T.PERIOD; T.LARROW], t)

and p_fwd_or_spawn_or_recv_or_shared st = match first st with
    T.RECV -> st |> shift @> p_id @> p_terminal T.SEMICOLON @> reduce r_action @> p_exp
  | T.ACQUIRE -> st |> shift @> p_id @> p_terminal T.SEMICOLON @> reduce r_action @> p_exp
  | T.ACCEPT -> st |> shift @> p_id @> p_terminal T.SEMICOLON @> reduce r_action @> p_exp
  | T.RELEASE -> st |> shift @> p_id @> p_terminal T.SEMICOLON @> reduce r_action @> p_exp
  | T.DETACH -> st |> shift @> p_id @> p_terminal T.SEMICOLON @> reduce r_action @> p_exp
  | T.IDENT(_id) -> st |> p_id @> p_fwd_or_spawn
  | t -> parse_error (here st, "expected 'recv', 'acquire', 'accept',
                                'release', 'detach' or identifier, found " ^ pp_tok t)

and p_fwd_or_spawn st = match first st with
    T.LARROW -> st |> shift @> push (Args ([], here st)) @> p_id_list_opt_exp
  | _t -> st |> reduce r_exp_atomic @> p_exp

and p_id_list_opt_exp st = match first st with
    T.IDENT(_id) -> st |> p_id @> reduce r_arg @> p_id_list_opt_exp
  | T.SEMICOLON -> st |> shift @> reduce r_action @> p_exp
  | _t -> st |> reduce r_exp_atomic @> p_exp

and r_arg st = match st with
    Tok(T.IDENT(id), r2) :: Args(args, r1) :: s -> s $ Args(args @ [id], join r1 r2)
  | _st -> raise UnknownParseError

(* [<idx>] postfix of action, default is 1 *)
and p_idx_opt st = match first st with
    T.SEMICOLON -> st |> push (Arith(R.Int(1), here st)) @> shift @> reduce r_action
  | T.LBRACE -> st |> p_idx_or_star @> p_terminal T.SEMICOLON @> reduce r_action
  | t -> error_expected_list (here st, [T.SEMICOLON; T.LBRACE], t)

(* reduce <exp>, where <exp> has no continuation (atomic expressions) *)
and r_exp_atomic st = match st with
    Tok(T.RPAREN,r2) :: Branches(branches) :: Tok(T.LPAREN,_) :: Tok(T.IDENT(id),_) :: Tok(T.CASE,r1) :: s ->
      s $ Exp(m_exp(A.Case(id,branches),join r1 r2),join r1 r2)
  | Tok(T.IDENT(id),r2) :: Tok(T.CLOSE,r1) :: s -> s $ Exp(m_exp(A.Close(id),join r1 r2),join r1 r2)
  | Args(args,r2) :: Tok(T.LARROW,_) :: Tok(T.IDENT(id),_) :: Tok(T.LARROW,_) :: Tok(T.IDENT(chan),r1) :: s ->
      s $ Exp(m_exp(A.ExpName(chan,id,args),join r1 r2),join r1 r2)
  | Tok(T.IDENT(id2),r2) :: Tok(T.LARROW,_) :: Tok(T.IDENT(id1),r1) :: s ->
      s $ Exp(m_exp(A.Fwd(id1,id2),join r1 r2),join r1 r2)
  | Tok(T.RPAREN,r2) :: Exp(exp,_r) :: Tok(T.LPAREN,r1) :: s ->
      s $ Exp(exp,join r1 r2)
  (* should be the only atomic expressions *)
  | _t -> raise UnknownParseError

(* reduce action prefix of <exp> *)
and r_action st = match st with
    Tok(T.SEMICOLON,r3) :: Tok(T.IDENT(id),r2) :: Tok(T.PERIOD,_) :: Tok(T.IDENT(chan),r1) :: s ->
      s $ Action((fun k -> m_exp(A.Lab(chan,id,k),join r1 r2)), join r1 r3)
  | Tok(T.SEMICOLON,r3) :: Tok(T.IDENT(chan),r2) :: Tok(T.WAIT,r1) :: s ->
      s $ Action((fun k -> m_exp(A.Wait(chan,k),join r1 r2)), join r1 r3)
  | Tok(T.SEMICOLON,r3) :: Tok(T.IDENT(chan2),r2) :: Tok(T.IDENT(chan1),_) :: Tok(T.SEND,r1) :: s ->
      s $ Action((fun k -> m_exp(A.Send(chan1,chan2,k),join r1 r2)), join r1 r3)
  | Tok(T.SEMICOLON,r3) :: Tok(T.IDENT(chan2),r2) :: Tok(T.RECV,_) :: Tok(T.LARROW,_) :: Tok(T.IDENT(chan1),r1) :: s ->
      s $ Action((fun k -> m_exp(A.Recv(chan2,chan1,k),join r1 r2)), join r1 r3)
  | Tok(T.SEMICOLON,r2) :: Arith(pot,_) :: Tok(T.WORK,r1) :: s ->
      s $ Action((fun k -> m_exp(A.Work(A.Arith pot,k),r1)), join r1 r2)
  | Tok(T.SEMICOLON,r2) :: Star(_) :: Tok(T.WORK,r1) :: s ->
      s $ Action((fun k -> m_exp(A.Work(A.Star,k),r1)), join r1 r2)
  | Tok(T.SEMICOLON,r3) :: Arith(pot,r2) :: Tok(T.IDENT(chan),_) :: Tok(T.PAY,r1) :: s ->
      s $ Action((fun k -> m_exp(A.Pay(chan,A.Arith pot,k),join r1 r2)), join r1 r3)
  | Tok(T.SEMICOLON,r3) :: Star(r2) :: Tok(T.IDENT(chan),_) :: Tok(T.PAY,r1) :: s ->
      s $ Action((fun k -> m_exp(A.Pay(chan,A.Star,k),join r1 r2)), join r1 r3)
  | Tok(T.SEMICOLON,r3) :: Arith(pot,r2) :: Tok(T.IDENT(chan),_) :: Tok(T.GET,r1) :: s ->
      s $ Action((fun k -> m_exp(A.Get(chan,A.Arith pot,k),join r1 r2)), join r1 r3)
  | Tok(T.SEMICOLON,r3) :: Star(r2) :: Tok(T.IDENT(chan),_) :: Tok(T.GET,r1) :: s ->
      s $ Action((fun k -> m_exp(A.Get(chan,A.Star,k),join r1 r2)), join r1 r3)
  | Tok(T.SEMICOLON,r3) :: Tok(T.IDENT(chan2),r2) :: Tok(T.ACQUIRE,_) :: Tok(T.LARROW,_) :: Tok(T.IDENT(chan1),r1) :: s ->
      s $ Action((fun k -> m_exp(A.Acquire(chan2,chan1,k),join r1 r2)), join r1 r3)
  | Tok(T.SEMICOLON,r3) :: Tok(T.IDENT(chan2),r2) :: Tok(T.ACCEPT,_) :: Tok(T.LARROW,_) :: Tok(T.IDENT(chan1),r1) :: s ->
      s $ Action((fun k -> m_exp(A.Accept(chan2,chan1,k),join r1 r2)), join r1 r3)
  | Tok(T.SEMICOLON,r3) :: Tok(T.IDENT(chan2),r2) :: Tok(T.RELEASE,_) :: Tok(T.LARROW,_) :: Tok(T.IDENT(chan1),r1) :: s ->
      s $ Action((fun k -> m_exp(A.Release(chan2,chan1,k),join r1 r2)), join r1 r3)
  | Tok(T.SEMICOLON,r3) :: Tok(T.IDENT(chan2),r2) :: Tok(T.DETACH,_) :: Tok(T.LARROW,_) :: Tok(T.IDENT(chan1),r1) :: s ->
      s $ Action((fun k -> m_exp(A.Detach(chan2,chan1,k),join r1 r2)), join r1 r3)
  | Tok(T.SEMICOLON,r3) :: Args(args,r2) :: Tok(T.LARROW,_) :: Tok(T.IDENT(id),_) :: Tok(T.LARROW,_) :: Tok(T.IDENT(chan),r1) :: s ->
      s $ Action((fun k -> m_exp(A.Spawn(chan,id,args,k),join r1 r2)), join r1 r3)
  | _t -> raise UnknownParseError

(* <branches> *)
and p_branches st = match first st with
    T.IDENT(_id) -> st |> shift @> p_terminal T.RRARROW @> p_exp @> reduce r_branch @> p_branches2
  | t -> error_expected (here st, T.IDENT("<label>"), t)

and p_branches2 st = match first st with
    T.RPAREN -> st (* branches complete; do not shift or reduce *)
  | T.BAR -> st |> drop @> p_branches
  | t -> error_expected_list (here st, [T.BAR; T.RPAREN], t)

(* reduce <branches> *)
and r_branch st = match st with
    Exp(exp,_r2) :: Tok(T.RRARROW,_) :: Tok(T.IDENT(id),r1) :: Branches(branches) :: s ->
    let branch = {A.lab_exp = (id,exp); A.exp_extent = PS.ext(r1)} in
    s $ Branches(branches @ [branch])
    (* should be the only possibility *)
  | _t -> raise UnknownParseError

(* <id> *)
and p_id st = match first st with
    T.IDENT(id) -> st |> drop @> push (Tok(T.IDENT(id), here st))
  | t -> parse_error (here st, "expected identifier, found " ^ pp_tok t)

(* parse any token (terminal symbol) 't_needed' *)
and p_terminal t_needed st = match first st with
    t -> if t_needed = t
	        then st |> shift
	        else error_expected (here st, t_needed, t)

(* (<pragma> | <decl>)* *)
let rec parse_decls token_front =
    let st = p_decl ([], token_front) in
    let () = if !ErrorMsg.anyErrors then raise ErrorMsg.Error else () in
    begin
      match st with
          ([], M.Cons((T.EOF, _r), _token_front)) -> []
          (* whole file processed *)
        | ([Decl(decl)], token_front) ->
          decl :: parse_decls token_front
          (* should be the only possibilities *)
        | _st -> raise UnknownParseError
    end;;

(* parse filename = decls
 * first apply lexer, the parser to the resulting token stream
 * raise ErrorMsg.Error if not lexically or syntactically correct
 * or if file does not exist or cannot be opened
 *)
let parse filename =
  try
    SafeIO.withOpenIn filename (fun instream ->
      let () = PS.pushfile filename (* start at pos 0 in filename *) in
      let n = in_channel_length instream in
      let s = Bytes.create n in
      let token_stream = Lex.makeLexer (really_input instream s 0 n ; Bytes.to_string s) in
      let decls = parse_decls (M.force token_stream) in
      let () = PS.popfile () in
      decls)
  with Sys_error e -> ( ErrorMsg.error_msg ErrorMsg.Parse None e
                           ; raise ErrorMsg.Error )

(* <pragma>*, ignoring remainder of token stream *)
let rec parse_preamble_decl token_front =
    let st = p_decl ([], token_front) (* may raise ErrorMsg.Error *)
    in try
      match st with
          ([Decl(dcl)], token_front) ->
            begin
              match dcl.A.declaration with
                A.Pragma("#test", _line) -> Some dcl
              | A.Pragma _
              | A.TpDef _ | A.TpEq _ | A.ExpDecDef _ | A.Exec _ -> parse_preamble_decl token_front
            end
        | _t -> raise ErrorMsg.Error
    with ErrorMsg.Error -> None;;

(* parse preamble = pragmas *)
let parse_preamble filename =
    SafeIO.withOpenIn filename (fun instream ->
      let () = PS.pushfile filename in
      let n = in_channel_length instream in
      let s = Bytes.create n in
      let token_stream = Lex.makeLexer (really_input instream s 0 n ; Bytes.to_string s) in
      let pragma = parse_preamble_decl (M.force token_stream) in
      let () = PS.popfile () in
      pragma)
    (* may raise IO.Io _, must be handled by caller *)

(* structure Parse *)
