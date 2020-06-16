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
let quoted_string = ref []
}

let newline = '\r' | '\n' | "\r\n"
let int = ['0'-'9'] ['0'-'9']*
let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?


rule token = parse

  (* white spaces *)
  | [ ' ' '\t' ]        { token lexbuf }

  (* imports *)
  | "import " (['a'-'z' 'A'-'Z' '/' '-' ' ' '.' '_']* as s) { IMPORT s }

  (* base types *)
  | "."                 { DOT }
  | int                 { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float               { FLOAT (float_of_string (Lexing.lexeme lexbuf))}
  
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

  (* probabilistic session types *)
  | "p+"                { POPLUS }
  | "p&"                { PAMPERSAND }
  
  (* functional types *)
  | "int"               { INTEGER }
  | "bool"              { BOOLEAN }
  | "address"           { ADDRESS }
  | "list"              { LIST }
  
  (* functional *)
  | newline             { next_line lexbuf; token lexbuf }
  | "(*"                { comment_depth := 1; comment lexbuf; token lexbuf }
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
  
  (* nomos specific *)
  | "Nomos.GetTxnNum()"       { GETTXNNUM }
  | "Nomos.GetTxnSender()"    { GETTXNSENDER }
  | "Nomos.MakeChannel"       { MAKECHAN }

  (* pnomos specific *)
  | "HH"                      { HH }
  | "TT"                      { TT }

  (* printing *)
  | "print"             { PRINT }    
  | '"'                 { quoted_string := [] ; printable_items lexbuf }

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
  | "close"             { CLOSE }
  | "wait"              { WAIT }
  | "work"              { WORK }
  | "pay"               { PAY }
  | "get"               { GET }
  | "acquire"           { ACQUIRE }
  | "accept"            { ACCEPT }
  | "release"           { RELEASE }
  | "detach"            { DETACH }
  | "abort"             { ABORT }

  (* probabilistic expressions *)
  | "pcase"             { PCASE }
  | "flip"              { FLIP }
  | ".."                { PDOT }

  (* identifier *)
  | ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* as word { ID (word) }
  | eof                 { EOF }
  | _                   { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

and comment = parse
    "(*"                { incr comment_depth; comment lexbuf }
  | "*)"                { decr comment_depth;
                          if !comment_depth = 0 then () else comment lexbuf }
  | eof                 { raise (SyntaxError "Unterminated comment") }
  | newline             { next_line lexbuf ; comment lexbuf }
  | _                   { comment lexbuf }

and printable_items = parse
    ' '                 { quoted_string := (Ast.Word(" "))::(!quoted_string); printable_items lexbuf }
  | '\t'                { quoted_string := (Ast.Word("\t"))::(!quoted_string); printable_items lexbuf }
  | '"'                 { QUOTED_STRING (!quoted_string) }
  | "%d"                { quoted_string := (Ast.PInt)::(!quoted_string); printable_items lexbuf }
  | "%b"                { quoted_string := (Ast.PBool)::(!quoted_string); printable_items lexbuf }
  | "%s"                { quoted_string := (Ast.PStr)::(!quoted_string); printable_items lexbuf }
  | "%a"                { quoted_string := (Ast.PAddr)::(!quoted_string); printable_items lexbuf }
  | "%c"                { quoted_string := (Ast.PChan)::(!quoted_string); printable_items lexbuf }
  | '\\' 'n'            { quoted_string := (Ast.PNewline)::(!quoted_string); printable_items lexbuf }
  | [^ '"' '\\' '%']+ as word { quoted_string := (Ast.Word(word))::(!quoted_string); printable_items lexbuf }
