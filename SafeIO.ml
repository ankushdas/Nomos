(* result of a computation *)
type 'a result = Value of 'a | Exception of exn

(* withOpenIn fileName (fn instream => body) = result
   opens fileName for input to obtain instream and evaluates body.
   The file is closed during normal and abnormal exit of body.
*)
let withOpenIn fileName scope =
   let instream = open_in fileName in
   (* val _ = fileOpenMsg (fileName) *)
   let result = try Value (scope instream) with exn -> Exception (exn) in
   (* val _ = fileCloseMsg (fileName) *)
   let _ = close_in instream in
   match result with
   | Value (x) -> x
   | Exception (exn) -> raise exn;;