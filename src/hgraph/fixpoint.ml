open Hgraph_types
open Fixpoint_types

let rec map_filter f l =
  match l with
  | [] -> []
  | t::q -> match f t with
    | None -> map_filter f q
    | Some t' -> t' :: (map_filter f q)

let lift_option_array a =
  try Some (Array.map (function
      | None -> raise Not_found
      | Some v -> v) a)
  with Not_found -> None

module Couple(L:OrderedHashedType)(R:OrderedHashedType) :
  OrderedHashedType with type t = L.t * R.t =
struct
  type t = (L.t * R.t)

  let hash (l,r) = Hashtbl.hash (L.hash l, R.hash r)

  let compare (l1,r1) (l2,r2) =
    let c = L.compare l1 l2 in
    if c <> 0 then c
    else R.compare r1 r2

  let equal (l1,r1) (l2,r2) =
    L.equal l1 l2 && R.equal r1 r2

  let print ppf (l,r) =
    Format.fprintf ppf "(@[<2>%a,@ %a@])" L.print l R.print r
end

module StackGraph (T:T) (H : Hgraph with module T := T) (Stack:Stack_types.Stack) = struct
  type stack = Stack.t

  module HedgeAndStack = Couple(T.Hedge)(Stack)
  module VertexAndStack = Couple(T.Vertex)(Stack)

  module HaS_Set = Set.Make(HedgeAndStack)
  module HaS_Map = Map.Make(HedgeAndStack)
  module VaS_Set = Set.Make(VertexAndStack)
  module VaS_Map = Map.Make(VertexAndStack)

  module StackMap = Map.Make(Stack)

  type 'a hedge_attrib = {
    orig_attrib: 'a;
    orig_hedge: T.hedge;
    stack : stack;
  }

  type 'a vertex_attrib = {
    orig_attrib: 'a;
    orig_vertex: T.vertex;
    stack : stack;
  }

  type ('v,'h,'e) g = {
    mutable orig_graph : ('v, 'h, 'e) H.graph StackMap.t;
    mutable call_sites : H.HedgeSet.t StackMap.t;
    graph : ('v vertex_attrib, 'h hedge_attrib, unit) H.graph;
    mutable hedges: T.hedge HaS_Map.t;
    mutable vertices: T.vertex VaS_Map.t;
  }

  let init_graph graph = {
    orig_graph = StackMap.singleton Stack.empty graph;
    call_sites = StackMap.empty;
    graph = H.create ();
    hedges= HaS_Map.empty;
    vertices= VaS_Map.empty;
  }

  let exists_call_site g stack hedge =
    try H.HedgeSet.mem hedge (StackMap.find stack g.call_sites)
    with Not_found -> false

  let add_call_site g stack hedge =
    let set =
      try StackMap.find stack g.call_sites
      with Not_found -> H.HedgeSet.empty in
    let set = H.HedgeSet.add hedge set in
    StackMap.add stack set g.call_sites

  let original_vertex g vertex =
    assert(H.contains_vertex g.graph vertex);
    let attrib = H.vertex_attrib g.graph vertex in
    attrib.orig_vertex

  let find_hedge g stack orig_hedge =
    try Some (HaS_Map.find (orig_hedge, stack) g.hedges) with
    | Not_found -> None

  let find_vertex g stack orig_vertex =
    try Some (VaS_Map.find (orig_vertex, stack) g.vertices) with
    | Not_found -> None

  let get_orig_graph g stack =
    assert(StackMap.mem stack g.orig_graph);
    StackMap.find stack g.orig_graph

  let make_vertex g stack orig_vertex =
    let orig_graph = get_orig_graph g stack in
    assert(H.contains_vertex orig_graph orig_vertex);
    let vertex = T.Vertex.clone orig_vertex in
    let orig_attrib = H.vertex_attrib orig_graph orig_vertex in
    let attrib = {
      orig_vertex;
      orig_attrib;
      stack } in
    H.add_vertex g.graph vertex attrib;
    g.vertices <- VaS_Map.add (orig_vertex, stack) vertex g.vertices;
    vertex

  let get_vertex g stack orig_vertex =
    match find_vertex g stack orig_vertex with
    | None -> make_vertex g stack orig_vertex
    | Some v -> v

  let make_hedge g stack orig_hedge =
    let orig_graph = get_orig_graph g stack in
    assert(H.contains_hedge orig_graph orig_hedge);
    let hedge = T.Hedge.clone orig_hedge in
    let orig_attrib = H.hedge_attrib orig_graph orig_hedge in
    let pred = Array.map (get_vertex g stack) (H.hedge_pred' orig_graph orig_hedge) in
    let succ = Array.map (get_vertex g stack) (H.hedge_succ' orig_graph orig_hedge) in
    let attrib = {
      orig_hedge;
      orig_attrib;
      stack } in
    H.add_hedge g.graph hedge attrib ~pred ~succ;
    g.hedges <- HaS_Map.add (orig_hedge, stack) hedge g.hedges;
    hedge

  let get_hedge g stack orig_hedge =
    match find_hedge g stack orig_hedge with
    | None -> make_hedge g stack orig_hedge
    | Some v -> v

  let hiset_map_hedge f set =
    H.HedgeIntSet.fold (fun (i,h) set -> H.HedgeIntSet.add (i,f h) set)
      set H.HedgeIntSet.empty

  (* get all successors of a vertex that correspond to successor of
     the original vertex *)
  let import_vertex_succ' g vertex =
    assert(H.contains_vertex g.graph vertex);
    let attrib = H.vertex_attrib g.graph vertex in
    let orig_graph = get_orig_graph g attrib.stack in
    assert(H.contains_vertex orig_graph attrib.orig_vertex);
    let orig_succ = H.vertex_succ' orig_graph attrib.orig_vertex in
    hiset_map_hedge (get_hedge g attrib.stack) orig_succ

  let import_vertex_pred' g vertex =
    assert(H.contains_vertex g.graph vertex);
    let attrib = H.vertex_attrib g.graph vertex in
    let orig_graph = get_orig_graph g attrib.stack in
    assert(H.contains_vertex orig_graph attrib.orig_vertex);
    let orig_pred = H.vertex_pred' orig_graph attrib.orig_vertex in
    hiset_map_hedge (get_hedge g attrib.stack) orig_pred

  let vertex_succ' g vertex : H.HedgeIntSet.t =
    ignore(import_vertex_succ' g vertex:H.HedgeIntSet.t);
    H.vertex_succ' g.graph vertex

  let vertex_pred' g vertex : H.HedgeIntSet.t =
    ignore(import_vertex_pred' g vertex:H.HedgeIntSet.t);
    H.vertex_pred' g.graph vertex

  let hset_of_hiset s = H.HedgeIntSet.fold (fun (_,v) set -> H.HedgeSet.add v set) s H.HedgeSet.empty

  let vertex_succ g v = hset_of_hiset (vertex_succ' g v)
  let vertex_pred g v = hset_of_hiset (vertex_pred' g v)

  let hedge_succ' g hedge =
    assert(H.contains_hedge g.graph hedge);
    (* edges have fixed successor in the graph: there is no need to
       import from the original graph *)
    H.hedge_succ' g.graph hedge

  let hedge_pred' g hedge =
    assert(H.contains_hedge g.graph hedge);
    H.hedge_pred' g.graph hedge

  let hset_of_array a = Array.fold_right H.VertexSet.add a H.VertexSet.empty

  let hedge_pred g h = hset_of_array (hedge_pred' g h)
  let hedge_succ g h = hset_of_array (hedge_succ' g h)

  let hedge_attrib g hedge =
    assert(H.contains_hedge g.graph hedge);
    (H.hedge_attrib g.graph hedge).orig_attrib

  type ('a,'b,'c) function_info =
    { stack_elt : Stack.elt;
      f_graph : ('a,'b,'c) H.graph;
      subgraph : H.subgraph }

  let import_function_call g hedge function_info =
    assert(H.contains_hedge g.graph hedge);
    let attrib = H.hedge_attrib g.graph hedge in
    let stack = Stack.push attrib.stack function_info.stack_elt in

    let link_function g hedge stack function_info =
      let input = hedge_pred' g hedge in
      let output = hedge_succ' g hedge in
      let clone_vertex orig_vertex =
        let vertex = T.Vertex.clone orig_vertex in
        g.vertices <- VaS_Map.add (orig_vertex, stack) vertex g.vertices;
        vertex
      in
      let clone_hedge orig_hedge =
        let hedge = T.Hedge.clone orig_hedge in
        g.hedges <- HaS_Map.add (orig_hedge, stack) hedge g.hedges;
        hedge
      in
      let _subgraph, _input_hedges, _output_hedges = H.clone_subgraph
          ~in_graph:function_info.f_graph
          ~out_graph:g.graph
          ~import_vattr:(fun ~old_vertex ~new_vertex:_ ~old_attr ->
              { orig_vertex = old_vertex;
                orig_attrib = old_attr;
                stack })
          ~import_hattr:(fun ~old_hedge ~new_hedge:_ ~old_attr ->
              { orig_hedge = old_hedge;
                orig_attrib = old_attr;
                stack })
          ~clone_vertex
          ~clone_hedge
          ~input
          ~output
          function_info.subgraph
      in
      g.orig_graph <- StackMap.add stack function_info.f_graph g.orig_graph
    in

    (* TODO: potentially make configurable to be able to not
       distinguish different hedges (less precise => probably faster) *)
    let new_call_site = not (exists_call_site g stack hedge) in
    if new_call_site
    then begin
      g.call_sites <- add_call_site g stack hedge;
      link_function g hedge stack function_info;
    end;
    new_call_site

end

module WorkQueue(S:Set.S) = struct
  type elt = S.elt
  type t = {
    queue : elt Queue.t;
    mutable set : S.t;
  }

  let push t v =
    if not (S.mem v t.set)
    then begin
      Queue.push v t.queue;
      t.set <- S.add v t.set
    end

  let push_set t s = S.iter (push t) s

  let pop t =
    if Queue.is_empty t.queue
    then None
    else
      let v = Queue.pop t.queue in
      t.set <- S.remove v t.set;
      Some v

  let create () =
    { queue = Queue.create (); set = S.empty }

end

module Fixpoint (T:T) (M:Manager with module T := T) = struct
  module SG = StackGraph(T) (M.H) (M.Stack)

  module Vwq = WorkQueue(M.H.VertexSet)
  module Hwq = WorkQueue(M.H.HedgeSet)

  type input_graph = (M.vertex_attribute,
                      M.hedge_attribute,
                      M.graph_attribute) M.H.graph

  type state =
    { graph : (M.vertex_attribute, M.hedge_attribute, M.graph_attribute) SG.g;
      vertex_values : M.abstract M.H.VertexMap.t;
      hedge_values : M.abstract array M.H.HedgeMap.t }

  let init_state graph =
    { graph = SG.init_graph graph;
      vertex_values = M.H.VertexMap.empty;
      hedge_values = M.H.HedgeMap.empty }

  let update_hedge state hedge =
    let pred = SG.hedge_pred' state.graph hedge in
    let f v =
      let abstract =
        try M.H.VertexMap.find v state.vertex_values with
        | Not_found -> M.bottom v in
      if M.is_bottom v abstract
      then None
      else Some abstract
    in
    match lift_option_array (Array.map f pred) with
    | None ->
      M.H.VertexSet.empty, state
    | Some abstract ->
      let attrib = SG.hedge_attrib state.graph hedge in
      let abstract, functions = M.apply hedge attrib abstract in
      let state = { state with hedge_values = M.H.HedgeMap.add hedge abstract state.hedge_values } in
      let functions = List.map (fun function_id ->
          let f_graph, subgraph = M.find_function function_id in
          { SG.stack_elt = function_id; f_graph; subgraph }) functions in
      let need_update =
        List.fold_left
          (fun need_update fun_info ->
             (* do side effects *)
             let update' = SG.import_function_call state.graph hedge fun_info in
             need_update || update') false functions in
      let to_update =
        if need_update
        then SG.hedge_pred state.graph hedge
        else M.H.VertexSet.empty in
      let to_update =
        M.H.VertexSet.union
          to_update
          (SG.hedge_succ state.graph hedge) in
      to_update, state


  let update_vertex narrowing_phase state vertex =
    let pred = M.H.HedgeIntSet.elements (SG.vertex_pred' state.graph vertex) in

    let hedge_opt state (i,h) =
      try Some (M.H.HedgeMap.find h state.hedge_values).(i) with
      | Not_found -> None
    in

    (* Union of the the values that can reach the state *)
    let abstract = match pred with
      | [] -> M.abstract_init (SG.original_vertex state.graph vertex)
      | _ ->
        match map_filter (hedge_opt state) pred with
        | [] -> M.bottom vertex
        | [a] -> a
        | l -> M.join_list vertex l
    in

    let previous_value =
      try Some (M.H.VertexMap.find vertex state.vertex_values) with
      | Not_found -> None in

    let propagate, abstract =
      match previous_value with
      | None -> true, abstract
      | Some previous_value ->
        (* if this is not the first value for this state, accelerate *)
        let abstract, propagate =
          (* if we are not in the narrowing phase: widen *)
          match narrowing_phase with
          | None ->
            let abstract = M.widening vertex previous_value abstract in
            let propagate = not (M.is_leq vertex abstract previous_value) in
            abstract, propagate
          | Some narrow ->
            let abstract = narrow vertex previous_value abstract in
            let propagate = not (M.is_leq vertex previous_value abstract) in
            abstract, propagate
        in
        propagate, abstract
    in

    let to_update =
      if propagate
      then SG.vertex_succ state.graph vertex
      else M.H.HedgeSet.empty
    in

    to_update, { state with vertex_values = M.H.VertexMap.add vertex abstract state.vertex_values }

  let loop narrowing_phase state start_vertices =
    let vwq = Vwq.create () in
    let hwq = Hwq.create () in
    Vwq.push_set vwq start_vertices;

    let rec aux state =
      match Vwq.pop vwq with
      | None ->
        begin match Hwq.pop hwq with
          | None -> state
          | Some h ->
            let to_update, state = update_hedge state h in
            Vwq.push_set vwq to_update;
            aux state
        end
      | Some v ->
        let to_update, state = update_vertex narrowing_phase state v in
        Hwq.push_set hwq to_update;
        aux state
    in
    aux state

  let kleene_fixpoint' graph start_vertices =
    let state = init_state graph in
    let start_vertices =
      M.H.VertexSet.fold (fun v set ->
          M.H.VertexSet.add
            (SG.make_vertex state.graph M.Stack.empty v)
            set) start_vertices
        M.H.VertexSet.empty in
    let state = loop None state start_vertices in
    match M.narrowing with
    | None -> state
    | Some _ ->
      let vertices_set =
        List.fold_left
          (fun set vertex -> M.H.VertexSet.add vertex set)
          M.H.VertexSet.empty
          (M.H.list_vertex state.graph.SG.graph) in
      loop M.narrowing state vertices_set

  let kleene_fixpoint (graph:input_graph) start_vertices =
    assert(M.H.correct graph);
    let state = kleene_fixpoint' graph start_vertices in
    let empty_vertex_map =
      List.fold_left (fun map orig_vertex ->
          M.H.VertexMap.add orig_vertex M.H.VertexSet.empty map)
        M.H.VertexMap.empty
        (M.H.list_vertex graph) in
    let vertex_map = ref empty_vertex_map in
    let map_vertex new_vertex attrib =
      let orig_vertex = attrib.SG.orig_vertex in
      let set =
        try M.H.VertexMap.find orig_vertex !vertex_map
        with Not_found -> M.H.VertexSet.empty in
      vertex_map :=
        M.H.VertexMap.add
          orig_vertex
          (M.H.VertexSet.add new_vertex set)
          !vertex_map;
      try M.H.VertexMap.find new_vertex state.vertex_values
      with Not_found -> M.bottom new_vertex
    in
    assert(M.H.correct state.graph.SG.graph);
    let graph = M.H.copy state.graph.SG.graph map_vertex (fun _ a -> a.SG.orig_attrib) (fun _ -> ()) in
    assert(M.H.correct graph);
    graph, !vertex_map

end
