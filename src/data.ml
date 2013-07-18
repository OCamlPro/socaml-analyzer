open Utils

(* use generative applications to have a new type each time *)
module Id = MakeId(struct end)
module Constant = MakeId(struct end)
module F = MakeId(struct end)

type id = Id.t

type constant = Constant.t

module Constants = Set.Make (struct type t = constant let compare = compare end)

type simple = Top | Constants of Constants.t

type tag = int

type f = F.t

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

let simple_bottom = Constants Constants.empty

let bottom =
  {
    top = false;
    int = simple_bottom;
    float = simple_bottom;
    string = simple_bottom;
    floata = simple_bottom;
    i32 = simple_bottom;
    i64 = simple_bottom;
    inat = simple_bottom;
    cp = Ints.empty;
    blocks = Tagm.empty;
    f = Fm.empty;
  }

let int_singleton const =
  { bottom with int = Constants (Constants.singleton const) }

let is_bottom_simple = function
  | Top -> false
  | Constants c -> Constants.is_empty c

let is_bottom { top; int; float; string; floata; i32;
                i64; inat; cp; blocks; f } =
  top = false && is_bottom_simple int && is_bottom_simple float &&
  is_bottom_simple string && is_bottom_simple floata &&
  is_bottom_simple i32 && is_bottom_simple i64 &&
  is_bottom_simple inat &&
  Ints.is_empty cp && Tagm.is_empty blocks && Fm.is_empty f

let union_simple a b = match a, b with
  | Top, _ | _, Top -> Top
  | Constants s, Constants s' -> Constants ( Constants.union s s')

let register_id (_:data) (_:data) = ()
let get_id (_:id) = bottom

let rec union a b =
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
    blocks =
      Tagm.merge
	begin
	  fun _ a b ->
	    match a, b with
	  | a, None | None, a -> a
	  | Some is1, Some is2 ->
	    Intm.merge
	      (fun _ a b ->
		match a, b with
		| a, None | None, a -> a
		| Some s1, Some s2 ->
		  Some (Array.mapi (fun i i1 -> union_id i1 s2.(i)))
	      )
	end
	a.merge b.merge;

    
    f = Fm.merge
      begin
	fun _ a b ->
	  match a, b with
	  | a, None | None, a -> a
	  | Some i1, Some i2 -> Some ( union_id i1 i2)
      end
      a.f b.f;

  }

and union_id i1 i2 =
  let i3 = create () in
  register_id i3 (union (get_id i1) (get_id i2));
  i3
  

let intersection_simple a b = match a, b with
  | Top, a | a, Top -> a
  | Constants s, Constants s' ->
    Constants ( Constants.inter s s')

let intersection a b =
  if a.top || b.top
  then union a b
    (* not completely sure this is the right thing to do for escape
       analysis, but this is correct for value analysis *)
  else
    { top = false;
    int = intersection_simple a.int b.int;
    float = intersection_simple a.float b.float;
    string = intersection_simple a.string b.string;
    floata = intersection_simple a.floata b.floata;
    i32 = intersection_simple a.i32 b.i32;
    i64 = intersection_simple a.i64 b.i64;
    inat = intersection_simple a.inat b.inat;
    cp = Ints.inter a.cp b.cp;
    blocks = a.blocks; (* This is WRONG ! Just wrong. To be changed. Kids might read it god damnit ! *)
    f = a.f;
  }

type environment =
  | Bottom
  | Env of data Idm.t

let is_bottom_env = function
  | Bottom -> true
  | _ -> false

let bottom_env = Bottom
let empty_env = Env Idm.empty

let set_env id data = function
  | Bottom ->
    (* not sure this should really forbidden, but this may help avoid
       some bugs *)
    failwith "bottom should never be assigned"
  | Env env -> Env (Idm.add id data env)

let get_env id = function
  | Bottom -> bottom
  | Env env ->
    try Idm.find id env
    with Not_found -> bottom
