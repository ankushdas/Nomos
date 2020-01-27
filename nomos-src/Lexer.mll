{
open Lexing
open Parser

exception SyntaxError of string


let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }

let comment_depth = ref 0
}

let newline = '\r' | '\n' | "\r\n"
rule token = parse
  (* declarations *)
  | "type"              { TYPE }
  | "proc"              { PROC }
  | "asset"             { ASSET }
  | "contract"          { CONTRACT }
  | "transaction"       { TRANSACTION }
  | "|-"                { TURNSTILE }
  | "exec"              { EXEC }
  | ":"                 { COLON }
  (* session types *)
  | "-o"                { LOLLI }
  | "&"                 { AMPERSAND }
  | "/\\"               { UP }
  | "\\/"               { DOWN }
  | "^"                 { PRODUCT }
  (* functional types *)
  | "int"               { INTEGER }
  | "bool"              { BOOLEAN }
  | "list"              { LIST }
  (* functional *)
  | [ ' ' '\t' ]        { token lexbuf }
  | newline             { next_line lexbuf; token lexbuf }
  | "(*"                { comment_depth := 1; comment lexbuf; token lexbuf }
  | ['0'-'9']+  as i    { INT (int_of_string i) }
  | "let"               { LET }
  | "in"                { IN }
  | "true"              { TRUE }  
  | "false"             { FALSE }
  | '+'                 { PLUS  }
  | '-'                 { MINUS }
  | '*'                 { TIMES }
  | '/'                 { DIV   }
  | '('                 { LPAREN }
  | ')'                 { RPAREN }
  | "["                 { LSQUARE }
  | "]"                 { RSQUARE }
  | "match"             { MATCH }
  | "fun"               { FUN   }
  | "with"              { WITH  }
  | "|"                 { BAR  }
  | "[]"                { EMPTYLIST }
  | "<>"                { NEQ     }
  | ">"                 { GREATER }
  | "<"                 { LESS }
  | ">="                { GREATEREQ }
  | "<="                { LESSEQ }
  | "&&"                { ANDALSO }
  | "||"                { ORELSE }
  | "->"                { RIGHTARROW }
  | "::"                { CONS }
  | ","                 { COMMA }
  | "if"                { IF }
  | "then"              { THEN }
  | "else"              { ELSE }
  | "="                 { EQUALS }

  (* session type channels *)
  | "#"                 { HASH }
  | "$"                 { DOLLAR }

  (* commands *)
  | "{"                 { LBRACE }
  | "}"                 { RBRACE }

  (* expressions *)
  | "<-"                { LARROW }
  | "recv"              { RECV }
  | ";"                 { SEMI }
  | "send"              { SEND }
  | "case"              { CASE }
  | "=>"                { RRARROW }
  | "."                 { DOT }
  | "close"             { CLOSE }
  | "wait"              { WAIT }
  | "work"              { WORK }
  | "pay"               { PAY }
  | "get"               { GET }
  | "acquire"           { ACQUIRE }
  | "accept"            { ACCEPT }
  | "release"           { RELEASE }
  | "detach"            { DETACH }

  (* identifier *)
  | ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* as word { ID (word) }
  | eof                 { EOF }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

and comment = parse
    "(*"                { incr comment_depth; comment lexbuf }
  | "*)"                { decr comment_depth;
                          if !comment_depth = 0 then () else comment lexbuf }
  | eof                 { raise (SyntaxError "Unterminated comment") }
  | newline             { next_line lexbuf ; comment lexbuf }
  | _                   { comment lexbuf }
