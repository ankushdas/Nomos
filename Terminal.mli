type terminal =
    LBRACE
  | RBRACE
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  | LANGLE
  | RANGLE
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
  | TURNSTILE
  | TYPE
  | EQTYPE
  | PROC
  | EXEC
  | IDENT of string
  | NAT of int
  | EOF
  | LEX_ERROR
  | PRAGMA of string * string
val toString : terminal -> string
