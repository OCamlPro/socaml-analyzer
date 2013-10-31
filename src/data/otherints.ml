open Data

type _ itype =
  | I32 : int32 itype
  | I64 : int64 itype
  | IN : nativeint

(* let singleton : type i. i itype -> i -> data = begin fun t i -> *)
(*   match t with *)
(*   | I32 -> let  *)
