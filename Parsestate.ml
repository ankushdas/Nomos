(* Parsing State *)
(* Imported from C0 compiler *)
(*
 * This tracks filename and newline characters
 * so character positions in lexer tokens
 * can be converted to line.column format for error messages
 *)


let currFilenames = ref ([] : string list)
let currLiness = ref ([] : int list list)

let reset () =
  currFilenames := []
  ; currLiness := [];;

let pushfile filename =
  currFilenames := filename::(!currFilenames)
  ; currLiness := []::!currLiness;;

let popfile () =
  currFilenames := List.tl (!currFilenames)
  ; currLiness := List.tl (!currLiness);;

let newline pos =
    (currLiness := (pos::(List.hd (!currLiness)))::(List.tl (!currLiness)));;

(* toolong max_col checks if the last line was too long *)
(* call only right after 'newline pos' *)
let linewidth () =
  match List.hd (!currLiness) with
    [] -> 0
  | [pos] -> pos
  | (last::prev::_rest) -> last-prev-1;; (* do not count '\n' character *)

(* look (pos, newline_positions, line_number) = (line, col)
* pos is buffer position
* newline_positions is (reverse) list of newline positions in file
* line_number is length of newline_positions
*)
let rec look (pos, l, n) = match l with
    [] ->
      (* in cc0: (1, pos-1) *)
      (* in ss: start lexer at pos = 1 *)
      (1, pos)
  | a :: rest ->
      (* a is end of line n *)
      if a < pos then (n+1, pos-a)
      else look (pos, rest, n-1);;

let last () = (List.length (List.hd (!currLiness)) + 1,  0);;

(* ext (leftpos, rightpos) = SOME((leftline, leftcol), (rightline, rightcol), filename)
* guess end of current file for invalid position (0,0)
*)
let ext (l, r) = match l, r with
    0, 0 -> (* NONE *)
    (* guess EOF, for potentially better error message? *)
      Some (last (), last (), List.hd (!currFilenames))
| left, right ->
      Some (look (left, List.hd (!currLiness), List.length (List.hd (!currLiness))),
      look (right, List.hd (!currLiness), List.length (List.hd (!currLiness))),
      List.hd (!currFilenames));;
