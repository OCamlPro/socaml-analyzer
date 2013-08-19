type _ datatype =
| Int -> simple datatype
| Float -> simple datatype

type data =
  
val empty : data
val find : 'a datatype -> data -> 'a
val set : 'a datatype -> 'a -> data -> data
val map : ('a datatype -> 'a -> 'a)
