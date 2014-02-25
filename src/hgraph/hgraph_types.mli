module type OrderedHashedType = sig
  type t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val print : Format.formatter -> t -> unit
end

module type T = sig
  type vertex (** Type of vertex identifiers *)
  type hedge  (** Type of hyperedge identifiers *)

  module Vertex : OrderedHashedType with type t = vertex
  module Hedge : OrderedHashedType with type t = hedge

  val print_vertex : Format.formatter -> vertex -> unit
  val print_hedge : Format.formatter -> hedge -> unit

end

module type Hgraph = sig
  module T : T

  module VertexSet : (Set.S with type t = Set.Make(T.Vertex).t and type elt = T.vertex)
  (** Set module for vertices *)
  module HedgeSet : (Set.S with type t = Set.Make(T.Hedge).t and type elt = T.hedge)
  (** Set module for hyperedges *)
  module VertexMap : (Map.S with type 'a t = 'a Map.Make(T.Vertex).t and type key = T.vertex)
  (** Set module for vertices *)
  module HedgeMap : (Map.S with type 'a t = 'a Map.Make(T.Hedge).t and type key = T.hedge)
  (** Set module for hyperedges *)
  module VertexTbl : (Hashtbl.S with type 'a t = 'a Hashtbl.Make(T.Vertex).t and type key=T.vertex)
  (** Hash module with vertices as keys *)
  module HedgeTbl : (Hashtbl.S with type 'a t = 'a Hashtbl.Make(T.Hedge).t and type key=T.hedge)
  (** Hash module with hyperedges as keys *)

  module HedgeInt : Set.OrderedType with type t = int * T.hedge
  module HedgeIntSet : (Set.S with type t = Set.Make(HedgeInt).t and type elt = int * T.hedge)

  type ('vattr, 'hattr, 'e) graph
  val create : ?size:int -> 'e -> ('vattr, 'hattr, 'e) graph
  val contains_vertex : (_, _, _) graph -> T.vertex -> bool
  val contains_hedge : (_, _, _) graph -> T.hedge -> bool
  val add_vertex : ('a, _, _) graph -> T.vertex -> 'a -> unit
  val add_hedge : (_, 'b, _) graph -> T.hedge -> 'b ->
    pred:T.vertex array -> succ:T.vertex array -> unit

  val vertex_succ : (_, _, _) graph -> T.vertex -> HedgeSet.t
  val vertex_pred : (_, _, _) graph -> T.vertex -> HedgeSet.t

  val hedge_succ : (_, _, _) graph -> T.hedge -> VertexSet.t
  val hedge_pred : (_, _, _) graph -> T.hedge -> VertexSet.t

  val vertex_succ' : (_, _, _) graph -> T.vertex -> HedgeIntSet.t
  val vertex_pred' : (_, _, _) graph -> T.vertex -> HedgeIntSet.t

  val hedge_succ' : (_, _, _) graph -> T.hedge -> T.vertex array
  val hedge_pred' : (_, _, _) graph -> T.hedge -> T.vertex array

  val import_hedge : (_, _, _) graph -> (_, 'b, _) graph -> T.hedge -> 'b -> unit
  val import_subgraph : (_, _, _) graph -> ('a, 'b, _) graph ->
    T.vertex list -> T.hedge list -> (T.vertex -> 'a) -> (T.hedge -> 'b) -> unit

  val list_vertex : (_, _, _) graph -> T.vertex list
  val list_hedge : (_, _, _) graph -> T.hedge list

  val vertex_attrib : ('a, _, _) graph -> T.vertex -> 'a
  val hedge_attrib : (_, 'b, _) graph -> T.hedge -> 'b

  val vertex_merge : ('a,_,_) graph -> ('a -> 'a -> 'a) -> T.vertex -> T.vertex -> unit
  (* vertex_merge g f v1 v2 merges v2 into v1, new attrib is f (vertex_attrib v1) (vertex_attrib v2) *)

  (* utils *)

  type subgraph = {
    sg_input : T.vertex array;
    sg_output : T.vertex array;
    sg_vertex : VertexSet.t;
    sg_hedge : HedgeSet.t;
  }

  val clone_subgraph :
    in_graph:('a, 'b, 'c) graph ->
    out_graph:('d, 'e, 'f) graph ->
    import_vattr:(new_vertex:T.vertex -> old_attr:'a -> 'd) ->
    import_hattr:(new_hedge:T.hedge -> old_attr:'b -> 'e) ->
    clone_vertex:(T.vertex -> T.vertex) ->
    clone_hedge:(T.hedge -> T.hedge) ->
    input:T.vertex array ->
    output:T.vertex array ->
    subgraph -> subgraph * HedgeIntSet.t array * HedgeIntSet.t array
  (** [clone_subgraph] returns a copy of the subgraph by copying the
      vertices and edges in out_graph. The vertex sg_input and
      sg_output are not copied, but are replaced by the provided input
      and output. There should be no link outside of the subgraph
      except in sg_input and sg_output. There must be no vertex from
      sg_input or sg_output in sg_vertex.

      The return value is : (subgraph, in_hedges, out_hedges)
  *)

  val print_dot :
    ?style:string ->
    ?titlestyle:string ->
    ?vertexstyle:string ->
    ?hedgestyle:string ->
    ?fvertexstyle:(T.vertex -> string) ->
    ?fhedgestyle:(T.hedge -> string) ->
    ?title:string ->
    ?print_attrvertex:(Format.formatter -> T.vertex -> 'a -> unit) ->
    ?print_attrhedge:(Format.formatter -> T.hedge -> 'b -> unit) ->
    Format.formatter -> ('a, 'b, 'c) graph -> unit

  val correct : ('a, 'b, 'c) graph -> bool

end
