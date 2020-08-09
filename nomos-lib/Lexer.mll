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
rule token = parse

  (* white spaces *)
  | [ ' ' '\t' ]        { token lexbuf }

  (* imports *)
  | "import " (['a'-'z' 'A'-'Z' '/' '-' ' ' '.' '_']* as s) { IMPORT s }
  
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
  | "coin"              { COIN }
  
  (* functional types *)
  | "int"               { INTEGER }
  | "bool"              { BOOLEAN }
  | "address"           { ADDRESS }
  | "list"              { LIST }
  
  (* functional *)
  | newline             { next_line lexbuf; token lexbuf }
  | "(*"                { comment_depth := 1; comment lexbuf; token lexbuf }
  | ['0'-'9']+ as i     { INT (int_of_string i) }
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
  | "."                 { DOT }
  | "close"             { CLOSE }
  | "wait"              { WAIT }
  | "work"              { WORK }
  | "deposit"           { DEPOSIT }
  | "pay"               { PAY }
  | "get"               { GET }
  | "acquire"           { ACQUIRE }
  | "accept"            { ACCEPT }
  | "release"           { RELEASE }
  | "detach"            { DETACH }
  | "abort"             { ABORT }

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
