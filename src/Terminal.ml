(* Terminal symbols *)
(* Interface between lexer and parser *)

type terminal =
         LBRACE | RBRACE | LPAREN | RPAREN
       | LBRACKET | RBRACKET | LANGLE | RANGLE
       | SLASH | BACKSLASH | UP | DOWN
       | COLON | COMMA | SEMICOLON | PERIOD
       | PLUS | MINUS | STAR | AMPERSAND | LOLLI
       | BAR | EQ | RRARROW | LARROW
       | CASE | CLOSE | WAIT | SEND | RECV
       | WORK | PAY | GET
       | ACQUIRE | ACCEPT | RELEASE | DETACH
       | TURNSTILE
       | TYPE | EQTYPE | PROC | EXEC
       | IDENT of string | NAT of int
       | EOF | LEX_ERROR
       | PRAGMA of string * string;; (* pragma and rest of line *)

let toString t = match t with
    LBRACE -> "{" | RBRACE -> "}" | LPAREN -> "(" | RPAREN -> ")"
  | LBRACKET -> "[" | RBRACKET -> "]" | LANGLE -> "<" | RANGLE -> ">"
  | SLASH -> "/" | BACKSLASH -> "\\" | UP -> "/\\" | DOWN -> "\\/"
  | COLON -> ":" | COMMA -> "," | SEMICOLON -> ";" | PERIOD -> "."
  | PLUS -> "+" | MINUS -> "-" | STAR -> "*" | AMPERSAND -> "&" | LOLLI -> "-o"
  | BAR -> "|" | EQ -> "=" | RRARROW -> "=>" | LARROW -> "<-"
  | CASE -> "case" | CLOSE -> "close" | WAIT -> "wait" | SEND -> "send" | RECV -> "recv"
  | WORK -> "work" | PAY -> "pay" | GET -> "get"
  | ACQUIRE -> "acquire" | ACCEPT -> "accept"
  | RELEASE -> "release" | DETACH -> "detach"
  | TURNSTILE -> "|-"
  | TYPE -> "type" | EQTYPE -> "eqtype" | PROC -> "proc" | EXEC -> "exec"
  | IDENT(s) -> s | NAT(n) -> string_of_int n
  | EOF -> "<eof>" | LEX_ERROR -> "<lex error>"
  | PRAGMA(pragma,line) -> pragma ^ line;;

(* structure Terminal *)