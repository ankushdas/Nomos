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


rule token = parse
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
  | "app"               { APP }
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
  | "int"               { INTEGER   }
  | "bool"              { BOOLEAN }
  | "list"              { LIST   }
  | ":"                 { COLON }
  | "->"                { RIGHTARROW }
  | "::"                { CONS }
  | ","                 { COMMA }
  | "if"                { IF }
  | "then"              { THEN }
  | "else"              { ELSE }
  | "="                 { EQUALS }
  | ['a'-'z' 'A'-'Z' '_']+ as word { ID (word) }
  | eof                 { EOF  }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

