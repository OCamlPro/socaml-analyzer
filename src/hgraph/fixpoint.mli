open Hgraph_types
open Fixpoint_types

module Fixpoint (T:T) (Manager:Manager with module T := T) : sig

  type input_graph = (Manager.vertex_attribute,
                      Manager.hedge_attribute,
                      Manager.graph_attribute)
      Manager.H.graph

  val kleene_fixpoint :
    (* ?err_graph:(unit, Manager.hedge_attribute, unit) Manager.H.graph option ref -> *)
    input_graph -> Manager.H.VertexSet.t ->
    (Manager.abstract, Manager.hedge_attribute, unit) Manager.H.graph *
    Manager.H.VertexSet.t Manager.H.VertexMap.t

  (* returns the unfolded graph and a map associating original vertex
     with new ones *)

end
