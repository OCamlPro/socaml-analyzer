open Data

type _ itype =
  | I32 : int32 itype
  | I64 : int64 itype
  | IN : nativeint itype

let singleton : type i. i itype -> i -> data = begin fun t i ->
  match t with 
  | I32 -> { bottom with i32 = Constants.singleton ( Int32.to_string i ) ; }
  | I64 -> { bottom with i64 = Constants.singleton ( Int64.to_string i ) ; }
  | IN -> { bottom with inat = Constants.singleton ( Nativeint.to_string i ) ; }
end
