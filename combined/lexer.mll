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
}

let newline = '\r' | '\n' | "\r\n"
let comment = "(*" ['a' - 'z' 'A' - 'Z']+ "*)"
rule token = parse
  (* declarations *)
  | "type"              { TYPE }
  | "proc"              { PROC }
  | "asset"             { ASSET }
  | "contract"          { CONTRACT }
  | "transaction"       { TRANSACTION }
  | "|-"                { TURNSTILE }
  | "exec"              { EXEC }
  (* types *)
  | "-o"                { LOLLI }
  | "&"                 { AMPERSAND }
  | "/\\"               { UP }
  | "\\/"               { DOWN }
  (* functional *)
  | [ ' ' '\t' ]        { token lexbuf }
  | newline             { next_line lexbuf; token lexbuf }
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
  | ";;"                { DOUBLESEMI }
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
  | ['a'-'z' 'A'-'Z' '_']+ as word { ID (word) }
  | eof                 { EOF }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

