(* Lexer *)
module PS = Parsestate
module T = Terminal
module M = TokStream

let error (lpos,rpos) msg = ( ErrorMsg.error_msg ErrorMsg.Lex (PS.ext(lpos,rpos)) msg
                            ; raise ErrorMsg.Error );;

let isAlpha c = match c with
    'a' .. 'z' -> true
  | 'A' .. 'Z' -> true
  | _c -> false;;

let isDigit c = match c with
    '0' .. '9' -> true
  | _c -> false;;

let id_start_char c =
  isAlpha c ||
  c = '$' ||
  c = '_' ||
  c = '\'';;

let id_char c =
  id_start_char c ||
  isDigit c;;

let rec implode l = match l with
    [] -> ""
  | x::xs -> Char.escaped x ^ implode xs;;

(* run_cond cond (n, accum, cs) = (string, n', cs')
 * accumulate characters in character stream 'cs' satisfying 'cond' in
 * 'accum' and return string, number of characters, and remaining
 * character stream
 *)
let rec run_cond cond (n, accum, cs) =
  match M.force cs with
    M.Nil -> (implode (List.rev accum), n, cs)
  | M.Cons (c, cs') ->
        if cond c
        then run_cond cond (n+1, c::accum, cs')
        else (implode (List.rev accum), n, cs);;

exception LexImpossible
(* lex_code (pos, charstream) = (token, lpos, rpos, cs') *)
(* token is the lexed token, [lpos,rpos) is the source region,
 * and cs' is the remaining character stream
 *)
let rec lex_code (pos, charstream) =
  match M.force charstream with
      M.Nil -> (T.EOF, pos, pos, charstream)
    (* Pragma *)
    | M.Cons ('#', cs) -> lex_pragma (pos+1, cs)
    (* Whitespace *)
    | M.Cons (' ', cs) -> lex_code (pos+1, cs)
    | M.Cons ('\t', cs) -> lex_code (pos+1, cs)
    | M.Cons ('\011', cs) -> lex_code (pos+1, cs)
    | M.Cons ('\r', cs) -> lex_code (pos+1, cs)
    | M.Cons ('\n', cs) ->
      ( PS.newline pos        (* track newlines for error messages *)
      ; lex_code (pos+1, cs) )
    (* Separators *)
    | M.Cons ('{', cs) -> (T.LBRACE, pos, pos+1, cs)
    | M.Cons ('}', cs) -> (T.RBRACE, pos, pos+1, cs)
    | M.Cons ('[', cs) -> (T.LBRACKET, pos, pos+1, cs)
    | M.Cons (']', cs) -> (T.RBRACKET, pos, pos+1, cs)
    | M.Cons (':', cs) -> (T.COLON, pos, pos+1, cs)
    | M.Cons (',', cs) -> (T.COMMA, pos, pos+1, cs)
    | M.Cons (';', cs) -> (T.SEMICOLON, pos, pos+1, cs)
    | M.Cons ('.', cs) -> (T.PERIOD, pos, pos+1, cs)
    | M.Cons ('+', cs) -> (T.PLUS, pos, pos+1, cs)
    | M.Cons ('-', cs) ->
        begin
          match M.force cs with
              M.Cons('o', cs) -> (T.LOLLI, pos, pos+2, cs)
            | _ -> (T.MINUS, pos, pos+1, cs)
        end
    | M.Cons ('*', cs) -> (T.STAR, pos, pos+1, cs)
    | M.Cons ('&', cs) -> (T.AMPERSAND, pos, pos+1, cs)
    (* Short sequences *)
    | M.Cons ('=', cs) ->
        begin
          match M.force cs with
              M.Cons('>', cs) -> (T.RRARROW, pos, pos+2, cs)
            | _ -> (T.EQ, pos, pos+1, cs)       (* using a wildcard here, sorry Jan! *)
        end
    | M.Cons ('<', cs) ->
        begin
          match M.force cs with
              M.Cons('-', cs) -> (T.LARROW, pos, pos+2, cs)
            | _ -> (T.LANGLE, pos, pos+1, cs)   (* using a wildcard here, sorry Jan! *)
        end
    | M.Cons ('>', cs) -> (T.RANGLE, pos, pos+1, cs)
    | M.Cons ('|', cs) ->
        begin
          match M.force cs with
            M.Cons('-', cs) -> (T.TURNSTILE, pos, pos+2, cs)
          | _ -> (T.BAR, pos, pos+1, cs)
        end
    | M.Cons ('%', cs) -> lex_comment_line (pos+1, cs)
    | M.Cons ('(', cs) ->
        begin
          match M.force cs with
            M.Cons('*', cs) -> lex_comment (pos+2, cs, 1)
          | _ -> (T.LPAREN, pos, pos+1, cs)
        end
    | M.Cons (')', cs) -> (T.RPAREN, pos, pos+1, cs)
    | M.Cons ('/', cs) ->
        begin
          match M.force cs with
            M.Cons('\\', cs) -> (T.UP, pos, pos+2, cs)
          | _ -> (T.SLASH, pos, pos+1, cs)
        end
    | M.Cons('\\', cs) ->
        begin
          match M.force cs with
            M.Cons('/', cs) -> (T.DOWN, pos, pos+2, cs)
          | _ -> (T.BACKSLASH, pos, pos+1, cs)
        end
    | M.Cons (c, _cs') ->
        begin
          if isDigit c
          then let (num_string, n, cs) = run_cond isDigit (0, [], charstream) in
               let num = int_of_string num_string in (* need to account for overflow! *)
               (T.NAT(num), pos, pos+n, cs)
          else if id_start_char c
          then
            begin
              match run_cond id_char (0, [], charstream) with
                ("case", n, cs) -> (T.CASE, pos, pos+n, cs)
              | ("close", n, cs) -> (T.CLOSE, pos, pos+n, cs)
              | ("wait", n, cs) -> (T.WAIT, pos, pos+n, cs)
              | ("send", n, cs) -> (T.SEND, pos, pos+n, cs)
              | ("recv", n, cs) -> (T.RECV, pos, pos+n, cs)
              | ("work", n, cs) -> (T.WORK, pos, pos+n, cs)
              | ("pay", n, cs) -> (T.PAY, pos, pos+n, cs)
              | ("get", n, cs) -> (T.GET, pos, pos+n, cs)
              | ("acquire", n, cs) -> (T.ACQUIRE, pos, pos+n, cs)
              | ("accept", n, cs) -> (T.ACCEPT, pos, pos+n, cs)
              | ("release", n, cs) -> (T.RELEASE, pos, pos+n, cs)
              | ("detach", n, cs) -> (T.DETACH, pos, pos+n, cs)
              | ("type", n, cs) -> (T.TYPE, pos, pos+n, cs)
              | ("eqtype", n, cs) -> (T.EQTYPE, pos, pos+n, cs)
              | ("proc", n, cs) -> (T.PROC, pos, pos+n, cs)
              | ("exec", n, cs) -> (T.EXEC, pos, pos+n, cs)
              | (ident, n, cs) -> (T.IDENT(ident), pos, pos+n, cs)
            end
          else error (pos, pos+1) ("illegal character: '" ^ Char.escaped c ^ "'")
        end

(* single-line comment % ... \n *)
and lex_comment_line (pos, charstream) =
    match M.force charstream with
        M.Nil -> (T.EOF, pos, pos, charstream)
      | M.Cons ('\n', cs) ->
        ( PS.newline pos
        ; lex_code (pos+1, cs) )
      | M.Cons (_c, cs) -> lex_comment_line (pos+1, cs)

(* single-line pragma #<pragma> ... *)
and lex_pragma (pos, charstream) =
  match run_cond id_char (1, ['#'], charstream) with
    ("#options", n, cs) ->
      begin
        match run_cond (fun c -> not (Char.equal c '\n')) (0, [], cs) with
          (* do not process newline *)
          (line, m, cs) -> (T.PRAGMA("#options", line), pos-1, pos-1+n+m, cs)
      end
  | ("#test", n, cs) ->
      begin
        match run_cond (fun c -> not (Char.equal c '\n')) (0, [], cs) with
          (line, m, cs) -> (T.PRAGMA("#test", line), pos-1, pos-1+n+m, cs)
      end
  | (s, n, _cs) -> error (pos-1, pos-1+n) ("unrecognized pragma: " ^ s)

(* delimited comment (* ... *) *)
and lex_comment (pos, charstream, depth) = (* depth >= 1 *)
    match M.force charstream with
        M.Nil -> error (pos, pos) ("unclosed delimited comment: reached end of file")
      | M.Cons('\n', cs) ->
          PS.newline pos
          ; lex_comment (pos+1, cs, depth)
      | M.Cons('*', cs) ->
          begin
            match M.force cs with
                M.Cons(')', cs) ->
                  if depth = 1 then lex_code (pos+2, cs)
                  else lex_comment (pos+2, cs, depth-1)
              | _ -> lex_comment (pos+1, cs, depth)
          end
      | M.Cons('(', cs) ->
          begin
            match M.force cs with
                M.Cons('*', cs) -> lex_comment (pos+2, cs, depth+1)
              | _ -> lex_comment (pos+1, cs, depth)
          end
      | M.Cons(_, cs) -> lex_comment (pos+1, cs, depth);;

exception Match
(* some infrastructure to allow strings, files, and
 * interactive streams to be lexed
 *)

let rec buffered_stream source =
  match source with
  | "" -> fun () -> M.Nil
  | s ->
      let c = String.get s 0 in
      fun () -> M.Cons(c, buffered_stream (String.sub s 1 (String.length s - 1)));;

(* lexresult = (token, (lpos, rpos)) *)
type lexresult = T.terminal * (int * int);;

let rec lexer (pos, charstream) =
  let (token, left_pos, right_pos, charstream) = lex_code (pos, charstream) in
  M.Cons ((token, (left_pos, right_pos)), fun () -> lexer (right_pos, charstream));;

(* start counting at pos = 1 for error messages *)
let makeLexer source = fun () -> lexer (1, buffered_stream source);;

(* struct Lex *)
