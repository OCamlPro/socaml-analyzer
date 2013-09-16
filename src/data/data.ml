(* The atomic types *)

open Utils
open Common_types
(* use generative applications to have a new type each time *)

module Constant = MakeId(struct end)

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

(* The data *)

type data =
  {
    top : bool;
    int : Int_interv.t;
    float : simple;
    string : simple;
    floata : simple;
    i32 : simple;
    i64 : simple;
    inat : simple;
    cp : Ints.t;
    blocks : Ids.t array Intm.t Tagm.t; (* referenced by tag, then by size *)
    f : Ids.t array Fm.t;
    expr : hinfo list;
  }

let simple_bottom = Constants Constants.empty

let bottom =
  {
    top = false;
    int = Int_interv.bottom;
    float = simple_bottom;
    string = simple_bottom;
    floata = simple_bottom;
    i32 = simple_bottom;
    i64 = simple_bottom;
    inat = simple_bottom;
    cp = Ints.empty;
    blocks = Tagm.empty;
    f = Fm.empty;
    expr = [];
  }

let top = { bottom with top = true; }

(* Bottom test *)

let is_bottom_simple = function
  | Top -> false
  | Constants c -> Constants.is_empty c

let is_bottom env { top; int; float; string; floata; i32;
                i64; inat; cp; blocks; f } =
  top = false && Int_interv.is_bottom int && is_bottom_simple float &&
  is_bottom_simple string && is_bottom_simple floata &&
  is_bottom_simple i32 && is_bottom_simple i64 &&
  is_bottom_simple inat &&
  Ints.is_empty cp && Tagm.is_empty blocks && Fm.is_empty f


(* Environment management *)

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

let reg_env data env =
  let i = Id.create () in
  ( set_env i data env, i)

let mem_env id = function
  | Bottom -> false
  | Env m -> Idm.mem id m

(* simple functions and values *)

(* ints *)

let restrict_int x = { bottom with int = x.int }

let restrict_not_int x =
  { x with int = bottom.int; cp = bottom.cp; }

let int_singleton const =
  { bottom with
    int = Int_interv.cst const;
  }
let any_int =
  { bottom with int = Int_interv.top;  }

let int_add x y =
  { bottom with int = Int_interv.add x.int y.int }

let int_op1 ( f : Int_interv.t -> Int_interv.t) x =
  { bottom with int = f x.int }

let int_op2 ( f : Int_interv.t -> Int_interv.t -> Int_interv.t) x y =
  { bottom with int = f x.int y.int }

let int_comp c x y =
  begin
    match Int_interv.comp c x.int y.int with
    | Some true -> { bottom with cp = Ints.singleton 1 }
    | Some false -> { bottom with cp = Ints.singleton 0 }
    | None -> { bottom with cp = Ints.add 1 ( Ints.singleton 0 ) }
  end,
    restrict_int x,
    restrict_int y

let int_make_comp c x y =
  let xi, yi = Int_interv.make_comp c x.int y.int in
  { x with int = xi }, { y with int = yi }

let cp_any i =
  let rec aux res = function
    | 0 -> res
    | n	-> let n = pred n in aux (Ints.add n res) n
  in { bottom with cp = aux Ints.empty i }

let cp_singleton i =
  { bottom with cp = Ints.singleton i }

let block_singleton tag content =
  { bottom with blocks = Tagm.singleton tag ( Intm.singleton ( Array.length content) ( Array.map Ids.singleton content) ) }

let has_cp v d = Ints.mem v d.cp
let is_one_cp d env =
  Ints.cardinal d.cp = 1 &&
  is_bottom env { d with cp = bottom.cp }

let restrict_cp ?v d =
  match v with
    Some v -> cp_singleton v
  | None -> { bottom with cp = d.cp }

let restrict_block ?tag ?has_field ?size d =
  let restrict_tag_size im =
    match has_field with
    | None -> im
    | Some f -> Intm.filter (fun k _ -> k > f) im
  in
  let restrict_tag im =
    match size with
    | None -> restrict_tag_size im
    | Some s -> Intm.singleton s ( Intm.find s im)
  in
  { bottom with blocks =
      match tag with
      | None -> Tagm.map restrict_tag d.blocks
      | Some t -> Tagm.singleton t ( restrict_tag ( Tagm.find t d.blocks))
  }

let has_tag t d = Tagm.mem t d.blocks
let is_one_tag d env =
  Tagm.cardinal d.blocks = 1 &&
  is_bottom env { d with blocks = bottom.blocks }

let set_a i v a =
  let a = Array.copy a in
  a.(i) <- v;
  a

let set_field i v b =
  let b = restrict_block ~has_field:i b in
  { b with blocks = Tagm.map ( Intm.map ( set_a i v)) b.blocks }


let get_field i b =
  Tagm.fold
    (fun _ b acc ->
      Intm.fold
	(fun s a acc ->
	  if s > i
	  then Ids.union acc a.(i)
	  else acc
	) b acc
    ) b.blocks Ids.empty

(* booleans *)
let booleans = (cp_any 2)

let restrict_bool x =
  { bottom with cp = Ints.inter x.cp booleans.cp }

let not_bool x =
  { bottom with cp =
      Ints.fold
	(fun i res ->
	  match i with
	  | 0 -> Ints.add 1 res
	  | 1 -> Ints.add 0 res
	  | _ -> res ) x.cp Ints.empty;
  }

(* expressions *)

let set_expression d e = { d with expr = [e]; }
let set_expressions d l = { d with expr = l; }
let add_expressions d e = { d with expr = e :: d.expr; }

(* Union *)

let union_simple a b = match a, b with
  | Top, _ | _, Top -> Top
  | Constants s, Constants s' -> Constants ( Constants.union s s')


let rec union (* env *) a b =
  let blocks =
    Tagm.merge
      begin
	fun _ a b ->
	  match a, b with
	  | a, None | None, a -> a
	  | Some is1, Some is2 ->
	    Some (
	      Intm.merge
		(fun _ a b ->
		  match a, b with
		  | a, None | None, a -> a
		  | Some s1, Some s2 -> Some ( Array.mapi (fun i i1 -> Ids.union i1 s2.(i)) s1)
		)
		is1 is2
	    )
      end
      a.blocks b.blocks in
  let f = Fm.merge
    begin
      fun _ a b ->
	match a, b with
	| a, None | None, a -> a
	| Some i1, Some i2 -> Some ( Array.mapi (fun i i1i -> Ids.union i1i i2.(i)) i1 )
    end
    a.f b.f;
  in
  (* env, *)
  {
    top = a.top || b.top;
    int = Int_interv.join a.int b.int;
    float = union_simple a.float b.float;
    string = union_simple a.string b.string;
    floata = union_simple a.floata b.floata;
    i32 = union_simple a.i32 b.i32;
    i64 = union_simple a.i64 b.i64;
    inat = union_simple a.inat b.inat;
    cp = Ints.union a.cp b.cp;
    blocks;
    f;
    expr = List.rev_append a.expr b.expr;
  }

and union_id env i1 i2 =
  let ( (* env, *) u) = union (* env *) (get_env i1 env) (get_env i2 env) in
  reg_env u env
  
let union_ids env ids = Ids.fold (fun a ( (* env, *) b) -> union (* env *) (get_env a env) b) ids ( (* env, *) bottom)

let fun_ids i env =
  Fm.fold (fun i _ l -> i::l ) (get_env i env).f []

(* simple access *)

(* let get_field f b env = *)
(*   Tagm.fold (fun _ sizes res -> *)
(*     Intm.fold (fun i vals res -> *)
(*       if i > f *)
(*       then union_id env (union_ids env vals.(f)) res *)
(*       else res) sizes res *)
(*   ) b.blocks bottom *)

(* Inclusion test *)

let included_simple a b = match a, b with
  | Top, _ | _, Top -> true
  | Constants s, Constants s' -> Constants.exists ( fun a -> Constants.mem a s') s

let array2_forall f a b =
  let l = Array.length a in
  let rec aux i = i = l || f a.(i) b.(i) || aux (succ i) in
  aux 0

let rec included env i1 i2 =
  let a = get_env i1 env
  and b = get_env i2 env in
  if is_bottom env b
  then is_bottom env a
  else 
    b.top
    || a.top
    || Int_interv.is_leq a.int b.int
    || included_simple a.float b.float
    || included_simple a.string b.string
    || included_simple a.floata b.floata
    || included_simple a.i32 b.i32
    || included_simple a.i64 b.i64
    || included_simple a.inat b.inat
    || Ints.exists (fun a -> Ints.mem a b.cp) a.cp
    || Tagm.exists
      (fun k a ->
	try
	  let b = Tagm.find k b.blocks in
	  Intm.exists
	    (fun k a ->
	      let b = Intm.find k b in
	      array2_forall
		(fun a b ->
		  Ids.exists
		    (fun a -> Ids.exists ( included env a) b)
		    a
		)
		a b
	    ) a
	with Not_found -> false) a.blocks
    || Fm.exists
      (fun k a ->
	try
	  let b = Fm.find k b.f in
	  array2_forall
	    (fun a b ->
	      Ids.exists
		(fun a -> Ids.exists ( included env a) b)
		a
	    ) a b
	with Not_found -> false) a.f

(* Leq test *)

let leq_simple a b =
  match a, b with
  | _, Top -> true
  | Top, _ -> false
  | Constants a, Constants b -> Constants.subset a b

let is_leq a b =
  b.top
  || begin
    not a.top
    && Int_interv.is_leq a.int b.int
    && leq_simple a.float b.float
    && leq_simple a.string b.string
    && leq_simple a.floata b.floata
    && leq_simple a.i32 b.i32
    && leq_simple a.i64 b.i64
    && leq_simple a.inat b.inat
    && Ints.subset a.cp b.cp
    && Tagm.for_all
      (fun k a ->
	try
	  let b = Tagm.find k b.blocks in
	  Intm.for_all
	    (fun k a ->
	      let b = Intm.find k b in
	      array2_forall Ids.subset a b
	    ) a
	with Not_found -> false
      ) a.blocks
    && Fm.for_all
      (fun k a ->
	try
	  let b = Fm.find k b.f in
	  array2_forall Ids.subset a b
	with Not_found -> false
      ) a.f
  end

(* Intersection *)

let intersection_simple a b = match a, b with
  | Top, a | a, Top -> a
  | Constants s, Constants s' ->
    Constants ( Constants.inter s s')


let intersect_noncommut env a b =
  (* keeps the ids in a that are possibly compatible with b *)
  if a.top then (env, b)
  else if b.top then (env, a)
  else
    let blocks = 
      Tagm.merge
	begin
	  fun _ a b ->
	    match a, b with
	    | _, None | None, _ -> None
	    | Some is1, Some is2 ->
	      Some (
		Intm.merge
		  (fun _ a b ->
		    match a, b with
		    | _, None | None, _ -> None
		    | Some s1, Some s2 ->
		      Some
			(
			  Array.mapi
			    (fun i i1 ->
			      ( Ids.filter (fun id -> Ids.exists ( included env id) s2.(i)) i1)
			    )
			    s1
			)
		  )
		  is1 is2
	      )
	end
	a.blocks b.blocks
    in
    let f =
      Fm.merge
	begin
	  fun _ a b ->
	    match a, b with
	    | _, None | None, _ -> None
	    | Some a, Some b ->
	      Some (
		Array.mapi
		  (fun i a ->
		    Ids.filter
		      (fun a ->
			Ids.exists (included env a) b.(i)
		      ) a
		  ) a
	      )
	end
	a.f b.f
    in
    env,
    { top = false;
      int = Int_interv.meet a.int b.int;
      float = intersection_simple a.float b.float;
      string = intersection_simple a.string b.string;
      floata = intersection_simple a.floata b.floata;
      i32 = intersection_simple a.i32 b.i32;
      i64 = intersection_simple a.i64 b.i64;
      inat = intersection_simple a.inat b.inat;
      cp = Ints.inter a.cp b.cp;
      blocks;
      f;
      expr = [];
    }
  
(* Environment joining *)

let join_env e1 e2 =
  match e1, e2 with
  | Bottom, e | e, Bottom -> e
  | Env i1, Env i2 ->
    Env
      ( Idm.merge
	    ( fun _ v1 v2 ->
	      match v1, v2 with
	      | None, v | v, None -> v
	      | Some v1, Some v2 ->
		Some (union v1 v2)
	    ) i1 i2
      )

(* Environment comparison *)

let is_leq_env e1 e2 =
  match e1, e2 with
  | Bottom, _ -> true
  | _, Bottom -> false
  | Env e1, Env e2 ->
    Idm.for_all (fun id d -> try is_leq d ( Idm.find id e2) with Not_found -> false ) e1

(* Garbage collection *)

let gc_env roots env =
  let dep_blocks b res =
    Tagm.fold (fun _ t res ->
      Intm.fold
	(fun _ a res ->
	  Array.fold_left (fun res ids -> List.rev_append (Ids.elements ids) res ) res a
	) t res
    ) b res
  and dep_funs f res =
    Fm.fold (fun _ a res ->
      Array.fold_left (fun res ids -> List.rev_append (Ids.elements ids) res ) res a
    ) f res
  and dep_expr l res =
    let rec aux res = function
      | [] -> res
      | e :: tl ->
        begin
	  match e with
   | Var x
   | Lazyforce x -> aux (x::res) tl
   | App ( x, y )
   | Send ( x, y ) -> aux ( x :: y :: res ) tl
   | Constraint _
   | Const _ -> aux res tl
   | Prim ( _, l )
   | Ccall ( _, l )->
     aux ( List.rev_append l res ) tl
        end
    in aux res l

  in
  let dependancies id idm =
    let d = Idm.find id idm in
    dep_blocks d.blocks ( dep_funs d.f ( dep_expr d.expr [] ) )
  in
  let rec add_with_dependants id idm res =
    if mem_env id res
    then res
    else
      let res = set_env id (Idm.find id idm) res in
      aux res idm (dependancies id idm)
  and aux res idm = function
    | [] -> res
    | id :: tl ->
      aux ( add_with_dependants id idm res ) idm tl
  in
  match env with
    Bottom -> Bottom
  | Env m -> aux empty_env m roots
    
