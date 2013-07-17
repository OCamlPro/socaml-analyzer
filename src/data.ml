type id = Ident.t

type constant = int

module Constants = Set.Make (struct type t = constant let compare = compare end)

type simple = Bottom | Top | Constants of Constants.t

type tag = int

type f = int

module Ints = Set.Make (struct type t = int let compare = compare end)
module Intm = Map.Make (struct type t = int let compare = compare end)

module Tagm = Map.Make (struct type t = tag let compare = compare end)

module Idm = Map.Make (struct type t = id let compare = compare end)
module Ids = Set.Make (struct type t = id let compare = compare end)

module Fm = Map.Make (struct type t = f let compare = compare end)

type data =
  {
    top : bool;
    int : simple;
    float : simple;
    string : simple;
    floata : simple;
    i32 : simple;
    i64 : simple;
    inat : simple;
    cp : Ints.t;
    blocks : Ids.t array Intm.t Tagm.t; (* referenced by tag, then by size *)
    f : id Fm.t;
  }

let bottom =
  {
    top = false;
    int = Bottom;
    float = Bottom;
    string = Bottom;
    floata = Bottom;
    i32 = Bottom;
    i64 = Bottom;
    inat = Bottom;
    cp = Ints.empty;
    blocks = Tagm.empty;
    f = Fm.empty;
  }

let union_simple a b =
| Bottom, a | a, Bottom -> a
| Top, _ | _, Top -> Top
| Constants s | Constants s' -> Constants ( Constants.union s s')

let union a b =
  {
    top = a.top || b.top;
    int = union_simple a.int b.int;
    float = union_simple a.float b.float;
    string = union_simple a.string b.string;
    floata = union_simple a.floata b.floata;
    i32 = union_simple a.i32 b.i32;
    i64 = union_simple a.i64 b.i64;
    inat = union_simple a.inat b.inat;
    cp = Ints.union a.cp b.cp;
    blocks = a.blocks; (* This is WRONG ! Just wrong. To be changed. Kids might read it god damnit ! *)
    f = a.f;
  }

    
