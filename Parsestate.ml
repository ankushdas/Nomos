(* Parsing State *)
(* Imported from C0 compiler *)
(*
 * This tracks filename and newline characters
 * so character positions in lexer tokens
 * can be converted to line.column format for error messages
 *)


let currFilenames = ref ([] : string list)
let currLines = ref ([] : int list list)

let reset () =
  currFilenames := []
  ; currLines := [];;

let pushfile filename =
  currFilenames := filename::(!currFilenames)
  ; currLines := []::!currLines;;

let popfile () =
  currFilenames := List.tl (!currFilenames)
  ; currLines := List.tl (!currLines);;

let newline pos =
    (currLines := (pos::(List.hd (!currLines)))::(List.tl (!currLines)));;

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

let last () = (List.length (List.hd (!currLines)) + 1,  0);;

(* ext (leftpos, rightpos) = SOME((leftline, leftcol), (rightline, rightcol), filename)
* guess end of current file for invalid position (0,0)
*)
let ext (l, r) = match l, r with
    0, 0 -> (* NONE *)
    (* guess EOF, for potentially better error message? *)
      Some (last (), last (), List.hd (!currFilenames))
| left, right ->
      Some (look (left, List.hd (!currLines), List.length (List.hd (!currLines))),
      look (right, List.hd (!currLines), List.length (List.hd (!currLines))),
      List.hd (!currFilenames));;
