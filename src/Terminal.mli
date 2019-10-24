(* Interface for Terminal *)

type terminal =
    LBRACE
  | RBRACE
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  | LANGLE
  | RANGLE
  | SLASH
  | BACKSLASH
  | UP
  | DOWN
  | COLON
  | COMMA
  | SEMICOLON
  | PERIOD
  | PLUS
  | MINUS
  | STAR
  | AMPERSAND
  | LOLLI
  | BAR
  | EQ
  | RRARROW
  | LARROW
  | CASE
  | CLOSE
  | WAIT
  | SEND
  | RECV
  | WORK
  | PAY
  | GET
  | ACQUIRE
  | ACCEPT
  | RELEASE
  | DETACH
  | TURNSTILE
  | TYPE
  | EQTYPE
  | PROC
  | EXEC
  | ASSET
  | CONTRACT
  | TRANSACTION
  | IDENT of string
  | NAT of int
  | EOF
  | LEX_ERROR
  | PRAGMA of string * string
val toString : terminal -> string
