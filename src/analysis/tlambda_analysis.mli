open Common_types
open Tlambda_to_hgraph

type v = Vertex.t

module type Entry =
sig
  val inv : v
  val outv : v
  val exnv : v
  val g : hg
  val funs : ( Data.f, fun_desc ) Hashtbl.t
  val mk_vertex : unit -> v
  val mk_hedge : unit -> Hedge.t
end

module M : functor ( E : Entry ) ->
  sig
    include Hgraph.Manager
      with
        module T := T
       and module H = G
       and type hedge_attribute = hattr
       and type vertex_attribute = vattr
       and type graph_attribute = gattr
       and type abstract = Data.environment
    val exn_tid : tid
  end
