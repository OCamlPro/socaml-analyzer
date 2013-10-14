open Data

(* expressions *)

let set d e = { d with expr = [e]; }
let sets d l = { d with expr = l; }
let add d e = { d with expr = e :: d.expr; }
