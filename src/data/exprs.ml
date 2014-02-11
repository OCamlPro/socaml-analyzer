open Data

(* expressions *)

let set d e = { d with expr = Hinfos.singleton e; }
let sets d l = { d with expr = List.fold_left (fun es e -> Hinfos.add e es) Hinfos.empty l; }
let add d e = { d with expr = Hinfos.add e d.expr; }
