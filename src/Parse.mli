(* Interface for Parser *)

module R = Arith
module A = Ast
module T = Terminal

type region = int * int
type prec = int
type stack_item =
    Tok of T.terminal * region
  | ArithInfix of prec * (R.arith * R.arith -> R.arith) * region
  | Arith of R.arith * region
  | Star of region
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

val parse : string -> A.decl_ext list
val parse_preamble : string -> A.decl_ext option
