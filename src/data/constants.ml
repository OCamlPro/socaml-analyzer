open Utils

module Constant = MakeId(struct end)

type t = Constant.t

module Constants = Set.Make (struct include Constant end)

type simple = Top | Constants of Constants.t


let tbl : ( t, string ) Hashtbl.t = Hashtbl.create 1024

let mk s =
  let c = Constant.create () in
  Hashtbl.add tbl c s;
  c

let singleton s =
  Constants ( Constants.singleton ( mk s ) )

let to_string = Hashtbl.find tbl
