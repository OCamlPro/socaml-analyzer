open Hgraph_types

module MakeT ( V : CloneOrderedHashedType ) ( H : CloneOrderedHashedType )
  : T with type vertex = V.t
       and type hedge = H.t
       and module Vertex = V
       and module Hedge = H =
struct
  type vertex = V.t
  type hedge = H.t
  module Vertex = V
  module Hedge = H

  let print_vertex = V.print
  let print_hedge = H.print
end

module Make(T:T) : Hgraph
  with module T := T
   and type VertexSet.elt = T.vertex
   and type VertexSet.t = Set.Make(T.Vertex).t
   and module VertexSet = Set.Make(T.Vertex)
   and type VertexMap.key = T.vertex
   and type 'a VertexMap.t = 'a Map.Make(T.Vertex).t
   and module VertexMap = Map.Make(T.Vertex)
   and type VertexTbl.key = T.vertex
   and module VertexTbl = Hashtbl.Make(T.Vertex)
   and type HedgeSet.elt = T.hedge
   and module HedgeSet = Set.Make(T.Hedge)
   and type HedgeMap.key = T.hedge
   and module HedgeMap = Map.Make(T.Hedge)
   and type HedgeTbl.key = T.hedge
   and module HedgeTbl = Hashtbl.Make(T.Hedge)

= struct

  open T
  module VertexSet = Set.Make(T.Vertex)
  module HedgeSet = Set.Make(T.Hedge)
  module VertexMap = Map.Make(T.Vertex)
  module HedgeMap = Map.Make(T.Hedge)
  module VertexTbl = Hashtbl.Make(T.Vertex)
  module HedgeTbl = Hashtbl.Make(T.Hedge)

  module HedgeInt = struct
    type t = int * hedge
    let compare ((i1:int),h1) (i2,h2) =
      let c = compare i1 i2 in
      if c <> 0
      then c
      else Hedge.compare h1 h2
  end

  module HedgeIntSet = Set.Make(HedgeInt)

  module VSet = VertexSet
  module VTbl = VertexTbl
  module HSet = HedgeSet
  module HTbl = HedgeTbl

  module HISet = HedgeIntSet

  let vset_of_array a = Array.fold_right VSet.add a VSet.empty
  (* let hset_of_array a = Array.fold_right HSet.add a HSet.empty *)

  (* module T = T *)

  type 'vattr vertex_n = {
    v_attr : 'vattr;
    mutable v_pred : HISet.t;
    mutable v_succ : HISet.t;
  }

  type 'hattr hedge_n = {
    h_attr : 'hattr;
    h_pred : vertex array;
    h_succ : vertex array;
  }

  type ('vattr,'hattr,'e) graph = {
    vertex : 'vattr vertex_n VTbl.t;
    hedge : 'hattr hedge_n HTbl.t;
    info : 'e
  }

  let create ?(size=20) info = {
    vertex = VTbl.create size;
    hedge = HTbl.create size;
    info;
  }

  let contains_vertex g v = VTbl.mem g.vertex v
  let contains_hedge g h = HTbl.mem g.hedge h

  let vertex_n g v = VTbl.find g.vertex v
  let hedge_n g h = HTbl.find g.hedge h

  let add_vertex_node g v n =
    if contains_vertex g v
    then failwith (Format.asprintf "add_vertex: the vertex %a is already \
                                    in the graph" Vertex.print v);
    VTbl.add g.vertex v n

  let add_hedge_node g h n =
    if contains_hedge g h
    then failwith (Format.asprintf "add_hedge: the hedge %a is already in \
                                    the graph" Hedge.print h);
    HTbl.add g.hedge h n

  let add_vertex g v v_attr =
    add_vertex_node g v { v_attr; v_pred = HISet.empty; v_succ = HISet.empty }

  let add_hedge g h h_attr ~pred ~succ =
    if contains_hedge g h
    then failwith (Format.asprintf "add_hedge: the hedge %a is already in \
                                    the graph" Hedge.print h);
    begin try
        Array.iteri
          (fun i v ->
             let vertex_n =  vertex_n g v in
             vertex_n.v_succ <- HISet.add (i,h) vertex_n.v_succ)
          pred;
        Array.iteri
          (fun i v ->
             let vertex_n =  vertex_n g v in
             vertex_n.v_pred <- HISet.add (i,h) vertex_n.v_pred)
          succ;
      with Not_found ->
        failwith "add_hedge: origin or destination vertex does not exists"
    end;
    HTbl.add g.hedge h
      { h_attr; h_pred = pred; h_succ = succ }

  let hset_of_hiset s = HISet.fold (fun (_,v) set -> HSet.add v set) s HSet.empty

  let vertex_succ' g v = (vertex_n g v).v_succ
  let vertex_pred' g v = (vertex_n g v).v_pred
  let vertex_succ g v = hset_of_hiset (vertex_succ' g v)
  let vertex_pred g v = hset_of_hiset (vertex_pred' g v)
  let hedge_succ' g h = Array.copy (hedge_n g h).h_succ
  let hedge_pred' g h = Array.copy (hedge_n g h).h_pred

  let hedge_succ g h = vset_of_array (hedge_succ' g h)
  let hedge_pred g h = vset_of_array (hedge_pred' g h)

  let list_vertex g = VTbl.fold (fun k _ l -> k::l) g.vertex []
  let list_hedge g = HTbl.fold (fun k _ l -> k::l) g.hedge []

  let vertex_attrib g v = (vertex_n g v).v_attr
  let hedge_attrib g h = (hedge_n g h).h_attr

  let import_hedge g1 g2 h attr =
    let hedge_n = hedge_n g1 h in
    add_hedge g2 h attr ~pred:hedge_n.h_pred ~succ:hedge_n.h_succ

  let vertex_merge g f v1 v2 =
    let v1n = vertex_n g v1
    and v2n = vertex_n g v2
    in
    let v_pred =
      HISet.fold (fun (i,h) set ->
          let hn = hedge_n g h in
          hn.h_succ.(i) <- v1;
          HISet.add (i,h) set) v2n.v_pred v1n.v_pred
    in
    let v_succ =
      HISet.fold (fun (i,h) set ->
          let hn = hedge_n g h in
          hn.h_pred.(i) <- v1;
          HISet.add (i,h) set) v2n.v_succ v1n.v_succ
    in
    VTbl.remove g.vertex v2;
    VTbl.remove g.vertex v1;
    add_vertex_node g v1
      { v_attr = f v1n.v_attr v2n.v_attr;
        v_pred; v_succ; }

  let import_subgraph g1 g2 vl hl fv fh =
    let import_vertex v = add_vertex g2 v (fv v) in
    let import_hedge h =
      let hedge_n = hedge_n g1 h in
      add_hedge g2 h (fh h) ~pred:hedge_n.h_pred ~succ:hedge_n.h_succ in
    List.iter import_vertex vl;
    List.iter import_hedge hl

  let copy g1 fv fh fe =
    let g2 = create (fe g1.info) in
    import_subgraph g1 g2
      (list_vertex g1)
      (list_hedge g1)
      (fun v -> fv v (vertex_attrib g1 v))
      (fun h -> fh h (hedge_attrib g1 h));
    g2

  type subgraph = {
    sg_input : vertex array;
    sg_output : vertex array;
    sg_vertex : VertexSet.t;
    sg_hedge : HedgeSet.t;
  }

  let clone_subgraph ~in_graph ~out_graph
      ~import_vattr ~import_hattr
      ~clone_vertex ~clone_hedge
      ~input ~output
      subgraph =
    let vertex_mapping = VTbl.create (VSet.cardinal subgraph.sg_vertex) in
    let hedge_mapping = HTbl.create (HSet.cardinal subgraph.sg_hedge) in

    if Array.length input <> Array.length subgraph.sg_input
    then raise (Invalid_argument "clone_subgraph: input and sg_input of different length");
    if Array.length output <> Array.length subgraph.sg_output
    then raise (Invalid_argument "clone_subgraph: output and sg_output of different length");

    let check_member_vertex s v =
      if not (VTbl.mem in_graph.vertex v)
      then raise (Invalid_argument
                    (Format.asprintf "clone_subgraph: vertex %a of %s is not in in_graph"
                       Vertex.print v s)) in
    Array.iter (check_member_vertex "sg_input") subgraph.sg_input;
    Array.iter (check_member_vertex "sg_output") subgraph.sg_output;
    VSet.iter (check_member_vertex "sg_vertex") subgraph.sg_vertex;

    let check_member_hedge h =
      if not (HTbl.mem in_graph.hedge h)
      then raise (Invalid_argument (Format.asprintf
                                      "clone_subgraph: hedge %a is not in in_graph"
                                      Hedge.print h)) in
    HSet.iter check_member_hedge subgraph.sg_hedge;

    let sg_input = Array.fold_right VSet.add subgraph.sg_input VSet.empty in
    let sg_output = Array.fold_right VSet.add subgraph.sg_output VSet.empty in
    let extended_vertex = VSet.union (VSet.union sg_input sg_output) subgraph.sg_vertex in
    (* extended_vertex are the vertex accepted as input or output of an edge *)

    Array.iter (fun sg_in ->
        if VSet.mem sg_in subgraph.sg_vertex
        then raise (Invalid_argument "clone_subgraph: sg_input and sg_vertex are not disjoint"))
      subgraph.sg_input;
    Array.iter (fun sg_out ->
        if VSet.mem sg_out subgraph.sg_vertex
        then raise (Invalid_argument "clone_subgraph: sg_output and sg_vertex are not disjoint"))
      subgraph.sg_output;

    Array.iteri (fun i sg_in  -> VTbl.add vertex_mapping sg_in  input.(i))  subgraph.sg_input;
    Array.iteri (fun i sg_out -> VTbl.add vertex_mapping sg_out output.(i)) subgraph.sg_output;

    let add_matching_hedge (i,h) set = HISet.add (i,HTbl.find hedge_mapping h) set in
    let matching_vertex v = VTbl.find vertex_mapping v in

    (* create a copy of hedges from g_in and associate the copy to the
       original in hedge_mapping *)
    let imported_hedge, assoc_hedge_list = HSet.fold (fun h (set,l) ->
        let new_hedge = clone_hedge h in
        HTbl.add hedge_mapping h new_hedge;
        HSet.add new_hedge set, (h, new_hedge)::l)
        subgraph.sg_hedge (HSet.empty,[]) in

    (* create a copy of vertex from g_in and associate the copy to the
       original in vertex_mapping. Then import the nodes in g_out *)
    let imported_vertex = VSet.fold (fun v set ->
        let new_vertex = clone_vertex v in
        VTbl.add vertex_mapping v new_vertex;
        let v_node = vertex_n in_graph v in
        if not (HSet.subset (hset_of_hiset v_node.v_pred) subgraph.sg_hedge) ||
           not (HSet.subset (hset_of_hiset v_node.v_succ) subgraph.sg_hedge)
        then raise (Invalid_argument "clone_subgraph: not independent subgraph");
        let new_node =
          { v_attr = import_vattr ~old_vertex:v ~new_vertex ~old_attr:v_node.v_attr;
            v_pred = HISet.fold add_matching_hedge v_node.v_pred HISet.empty;
            v_succ = HISet.fold add_matching_hedge v_node.v_succ HISet.empty } in
        add_vertex_node out_graph new_vertex new_node;
        VSet.add new_vertex set) subgraph.sg_vertex VSet.empty in

    (* import hedge nodes in out_graph *)
    List.iter (fun (old_h, new_h) ->
        let h_node = hedge_n in_graph old_h in
        begin
          let check v = if not (VSet.mem v extended_vertex)
            then raise (Invalid_argument "clone_subgraph: not independent subgraph") in
          Array.iter check h_node.h_pred;
          Array.iter check h_node.h_succ
        end;
        let new_node =
          { h_attr = import_hattr ~old_hedge:old_h ~new_hedge:new_h ~old_attr:h_node.h_attr;
            h_pred = Array.map matching_vertex h_node.h_pred;
            h_succ = Array.map matching_vertex h_node.h_succ } in
        add_hedge_node out_graph new_h new_node) assoc_hedge_list;

    (* import links for input and output vertices *)
    let recover_link vert =
      let new_vert = matching_vertex vert in
      let vert_node = vertex_n in_graph vert in
      let new_vert_node = vertex_n out_graph new_vert in
      new_vert_node.v_pred <-
        HISet.fold add_matching_hedge vert_node.v_pred
          new_vert_node.v_pred;
      new_vert_node.v_succ <-
        HISet.fold add_matching_hedge vert_node.v_succ
          new_vert_node.v_succ;
      new_vert
    in

    let in_hedges = Array.map (fun vert ->
        let vert_node = vertex_n in_graph vert in
        HISet.fold add_matching_hedge vert_node.v_succ HISet.empty)
        subgraph.sg_input in
    let out_hedges = Array.map (fun vert ->
        let vert_node = vertex_n in_graph vert in
        HISet.fold add_matching_hedge vert_node.v_pred HISet.empty)
        subgraph.sg_output in

    { sg_input  = Array.map recover_link subgraph.sg_input;
      sg_output = Array.map recover_link subgraph.sg_output;
      sg_vertex = imported_vertex;
      sg_hedge  = imported_hedge }, in_hedges, out_hedges

  let correct g =
    let check b = if b then () else raise Exit in
    let correct_vertex v _ =
      HISet.iter (fun (i,h) -> check (Vertex.equal (hedge_succ' g h).(i) v))
        (vertex_pred' g v);
      HISet.iter (fun (i,h) -> check (Vertex.equal (hedge_pred' g h).(i) v))
        (vertex_succ' g v)
    in
    let correct_hedge h _ =
      Array.iteri (fun i v -> check (HISet.mem (i,h) (vertex_succ' g v)))
        (hedge_pred' g h);
      Array.iteri (fun i v -> check (HISet.mem (i,h) (vertex_pred' g v)))
        (hedge_succ' g h)
    in
    HTbl.iter correct_hedge g.hedge;
    VTbl.iter correct_vertex g.vertex

  let correct g =
    try correct g; true
    with _ -> false

  let print_dot
      ?(style:string="")
      ?(titlestyle:string="shape=plaintext,style=bold,style=filled,fontsize=12")
      ?(vertexstyle:string="shape=box,fontsize=12")
      ?(hedgestyle:string="shape=ellipse,fontsize=12")
      ?fvertexstyle
      ?fhedgestyle
      ?(title:string="")
      ?print_attrvertex
      ?print_attrhedge
      ppf g
    =
    let open Format in
    fprintf ppf "digraph G {@.  @[<v>%s@ " style;
    if title<>"" then
      fprintf ppf "1073741823 [%s,label=\"%s\"];@ " titlestyle title;
    begin match print_attrvertex with
      | None -> ()
      | Some print_attrvertex ->
        VTbl.iter
          (begin fun vertex vertex_n ->
             fprintf ppf "%a [%s,label=\"%t\"];@ "
               print_vertex vertex
               (match fvertexstyle with
                | Some f -> f vertex
                | None -> vertexstyle)
               (fun ppf -> print_attrvertex ppf vertex vertex_n.v_attr);
           end)
          g.vertex
    end;
    begin match print_attrhedge with
      | None -> ()
      | Some print_attrhedge ->
        HTbl.iter
          (begin fun hedge hedge_n ->
             fprintf ppf "%a [%s,label=\"%t\"];@ "
               print_hedge hedge
               (match fhedgestyle with
                | Some f -> f hedge
                | None -> hedgestyle)
               (fun ppf -> print_attrhedge ppf hedge hedge_n.h_attr);
           end)
          g.hedge
    end;
    HTbl.iter
      (begin fun hedge hedge_n ->
         Array.iter
           (begin fun pred ->
              fprintf ppf "\"%a\" -> \"%a\";@ "
                print_vertex pred print_hedge hedge
            end)
           hedge_n.h_pred;
         Array.iter
           (begin fun succ ->
              fprintf ppf "\"%a\" -> \"%a\";@ "
                print_hedge hedge print_vertex succ
            end)
           hedge_n.h_succ
       end)
      g.hedge
    ;
    fprintf ppf "@]@.}@.";
    ()

end

