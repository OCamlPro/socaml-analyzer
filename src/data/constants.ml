open Utils

module C = MakeId(struct end) 

let tbl : ( C.t, string ) Hashtbl.t = Hashtbl.create 1024

let to_string = Hashtbl.find tbl

module Constant =
struct
  include C
  let print pp c = Format.pp_print_string pp (to_string c)
end
type t = Constant.t

module Constants = Set.Make (struct include Constant end)

type simple = Top | Constants of Constants.t



let mk s =
  let c = Constant.create () in
  Hashtbl.add tbl c s;
  c

let singleton s =
  Constants ( Constants.singleton ( mk s ) )
