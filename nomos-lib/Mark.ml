open Sexplib.Std

(* ((line1, col1), (line2, col2), filename) : ext *)
(* inclusive on left, exclusive on right *)
type ext = (int * int) * (int * int) * string [@@deriving sexp]

(* col 0 means no column info, just show line number *)
let pos (line, col) = match col with
  | 0 -> string_of_int line
  | col -> string_of_int line ^ "." ^ string_of_int col;;

let show (left, right, file) = file ^ ":" ^ pos left ^ "-" ^ pos right;;

let theLine line = String.sub line 0 (String.length(line));;

let rec inputLines n instream = match n with
  | 0 -> None
  | 1 -> Some (theLine (input_line instream))
  | n -> let _ = input_line instream in (* ignore line *)
          inputLines (n-1) instream;;

let rec createLine col1 col2 = match col1, col2 with
  | _, 0 -> ""
  | _, 1 -> " "
  | 0, col2 -> "~" ^ createLine 0 (col2-1)
  | 1, col2 -> "~" ^ createLine 1 (col2-1)
  | col1, col2 -> " " ^ createLine (col1-1) (col2-1);;

let rec count_whitespace i s =
  if i < String.length(s) && Char.equal (String.get s i) ' '
  then count_whitespace (i+1) s
  else i;;

let show_source ((line1, col1), (line2, col2), file) =
  try SafeIO.withOpenIn file (fun instream -> 
  match inputLines line1 instream with
    | None -> "[location at end of file]\n"
    | Some first_line ->
      if line1 = line2
      then first_line ^ "\n" ^ createLine col1 col2 ^ "\n"
      else let second_line = (match inputLines (line2-line1) instream with
                              | None -> "<eof>"
                              | Some (line) -> line)
            in
            let ws_count = count_whitespace 0 second_line in
            let error_line = first_line ^ " ... "
                            ^ String.sub second_line ws_count (String.length(second_line) - ws_count) in
            let indicator = createLine col1 (String.length first_line + 5 + col2 - ws_count) in
            error_line ^ "\n" ^ indicator ^ "\n")

  with Sys_error s -> s;;

type 'a marked = 'a * ext option

let mark' (x, ext_opt) = (x, ext_opt);;

let data (x, _ext_opt) = x;;
let ext (_x, ext_opt) = ext_opt;;
