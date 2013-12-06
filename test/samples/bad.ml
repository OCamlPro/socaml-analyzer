let arg = "Please make sure that this file WILL raise an exception"

module M = Pervasives

(* let f = M.print_endline *)
(* let a = print_endline arg *)

let _ = assert false
