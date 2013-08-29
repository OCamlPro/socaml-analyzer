open Utils

module Id =
struct
  open Ident
  type t = Ident.t
  let compare = compare
  let name x = Some x.name
  let to_string x = Printf.sprintf "%s/%d" x.name x.stamp
  let output o x = Printf.fprintf o "%s/%d" x.name x.stamp
  let print = print
  let idref = ref 0
  let create ?(name="") () =
    decr idref;
    { stamp = !idref; name = ( "$$" ^ name ); flags = 0; }
end

module F = MakeId(struct end)
