(* The atomic types *)
open Utils
open Common_types

(* use generative applications to have a new type each time *)

type id = tid

type constant = Constants.t

open Constants

type tag = int

type f = F.t

module Ints = Set.Make (struct type t = int let compare = compare let print = Format.pp_print_int end)
module Intm = Map.Make (struct type t = int let compare = compare let print = Format.pp_print_int end)

module Tagm = Map.Make (struct type t = tag let compare = compare let print = Format.pp_print_int end)

module Idm = Map.Make (struct type t = tid let compare = compare let print = TId.print end)
module Ids = Set.Make (struct type t = tid let compare = compare let print = TId.print end)

module Fm = Map.Make (struct type t = f let compare = compare let print = F.print end)

type array_descr =
  {
    a_elems: Ids.t;
    a_size: Int_interv.t;
  }

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
    arrays : array_descr;
    f : Ids.t array Fm.t;
    expr : hinfo list;
  }

(* The environment *)
type environment =
  | Bottom
  | Env of data Idm.t

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
    arrays = { a_elems = Ids.empty; a_size = Int_interv.bottom; };
    f = Fm.empty;
    expr = [];
  }

let top = { bottom with top = true; }

(* Bottom test *)

let is_bottom_simple = function
  | Top -> false
  | Constants c -> Constants.is_empty c

let is_bottom env { top; int; float; string; floata; i32;
                    i64; inat; cp; blocks; arrays = { a_elems; a_size; }; f; } =
  top = false && Int_interv.is_bottom int && is_bottom_simple float &&
  is_bottom_simple string && is_bottom_simple floata &&
  is_bottom_simple i32 && is_bottom_simple i64 &&
  is_bottom_simple inat &&
  Ints.is_empty cp && Tagm.is_empty blocks && ( Ids.is_empty a_elems || Int_interv.is_bottom a_size ) && Fm.is_empty f

(* basic env management *)

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
  let i = TId.create () in
  ( set_env i data env, i)

let mem_env id = function
  | Bottom -> false
  | Env m -> Idm.mem id m



(* simple functions and values *)



let set_a i v a =
  let a = Array.copy a in
  a.(i) <- v;
  a

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
    arrays =
      {
        a_elems = Ids.union a.arrays.a_elems b.arrays.a_elems;
        a_size = Int_interv.join a.arrays.a_size b.arrays.a_size;
      };
    f;
    expr = List.rev_append a.expr b.expr;
  }

and union_id env i1 i2 =
  let ( (* env, *) u) = union (* env *) (get_env i1 env) (get_env i2 env) in
  reg_env u env

let union_ids env ids = Ids.fold (fun a ( (* env, *) b) -> union (* env *) (get_env a env) b) ids ( (* env, *) bottom)

let fun_ids i env =
  Fm.fold (fun i _ l -> i::l ) (get_env i env).f []

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
    || ( Int_interv.is_leq a.arrays.a_size b.arrays.a_size && true (* TODO: the idsets *) )
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
    && Ids.subset a.arrays.a_elems b.arrays.a_elems
    && Int_interv.is_leq a.arrays.a_size b.arrays.a_size
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
      arrays =
        {
          a_elems = b.arrays.a_elems (* TODO: see that again *);
          a_size = Int_interv.meet a.arrays.a_size b.arrays.a_size;
        };
      f;
      expr = [];
    }

let odo f g = function
  | Some x -> f x
  | None -> g ()

let sep pp = Format.fprintf pp ",@ "

let print_simple pp t =
  let open Format in
  function
  | Top -> fprintf pp "@[%s: Top@]@." t
  | Constants s when Constants.is_empty s -> ()
  | Constants s ->
    fprintf pp "%s: [@ @[" t;
    Constants.print_sep sep pp s;
    fprintf pp "@]@ ]@."

let print_ids_array pp a = ()
(* let open Format in *)
(* let l = Array.length a  in *)
(* if l = 0 *)
(* then ( pp_print_string pp "[||]"; Ids.empty ) *)
(* else ( *)
(*   fprintf pp "[|@ @[@ %a"; *)
(*   Array.iter *)
(*     (fun ids -> *)
(*        failwith "pretty-printing ids" *)
(*     ) *)
(*     a; *)
(*   fprintf pp "@]@ |]" *)
(* ) *)

let print pp id env =
  let open Format in
  TId.print pp id;
  fprintf pp ":@[";
  pp_open_box pp 0;
  let d = get_env id env in
  begin
    if d.top
    then ( fprintf pp "Top@.")
    else
      begin
        if not ( Int_interv.is_bottom d.int )
        then
          (
            fprintf pp "Ints: [@ @[";
            odo
              (pp_print_int pp)
              (fun () -> pp_print_string pp "-inf")
              (Int_interv.lower d.int);
            fprintf pp ",@ ";
            odo
              (pp_print_int pp)
              (fun () -> pp_print_string pp "inf")
              (Int_interv.higher d.int);
            fprintf pp "@]@ ]@."
          );
        print_simple pp "Floats" d.float;
        print_simple pp "Strings" d.string;
        print_simple pp "Float arrays" d.floata;
        print_simple pp "Int32" d.i32;
        print_simple pp "Int64" d.i64;
        print_simple pp "Native ints" d.inat;
        if not ( Ints.is_empty d.cp )
        then
          (
            fprintf pp  "Const pointers : [ @[@ ";
            Ints.print_sep sep pp d.cp;
            fprintf pp "@]@ ]@."
          );
        if not ( Tagm.is_empty d.blocks )
        then
          (
            fprintf pp "Blocks@.@[@ ";
            Tagm.print
              (fun pp im ->
                 fprintf pp "@[{@ ";
                 Intm.print_sep
                   sep
                   print_ids_array
                   pp 
                   im;
                 fprintf pp "}@]@." )
              pp
              d.blocks;
            fprintf pp "@ @]@."
          );
        if not ( Ids.is_empty d.arrays.a_elems || Int_interv.is_bottom d.arrays.a_size )
        then
          (
            fprintf pp "Arrays@.[@[@ sizes:@[";
            Int_interv.print pp d.arrays.a_size;
            fprintf pp "@]@ elements:@[";
            Ids.print_sep sep pp d.arrays.a_elems;
            fprintf pp "@]@ @]]@."
          );
        if not ( Fm.is_empty d.f )
        then
          (
            fprintf pp "Functions: {@[@ ";
            Fm.print_sep sep (fun _ _ -> ()) pp d.f;
            fprintf pp "@ @]}@."
          )
      end
  end;
  fprintf pp "@]@."
