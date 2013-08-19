module type OrderedHashedType = sig
  type t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
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

module Make(T:T) : Hgraph with module T = T = struct
  module T = T
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
  let hset_of_array a = Array.fold_right HSet.add a HSet.empty

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
    then failwith "add_vertex: the vertex is already in the graph";
    VTbl.add g.vertex v n

  let add_hedge_node g h n =
    if contains_hedge g h
    then failwith "add_hedge: the hedge is already in the graph";
    HTbl.add g.hedge h n

  let add_vertex g v v_attr =
    add_vertex_node g v { v_attr; v_pred = HISet.empty; v_succ = HISet.empty }

  let add_hedge g h h_attr ~pred ~succ =
    if contains_hedge g h
    then failwith "add_hedge: the hedge is already in the graph";
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
  let hedge_succ' g h = (hedge_n g h).h_succ
  let hedge_pred' g h = (hedge_n g h).h_pred

  let hedge_succ g h = vset_of_array (hedge_succ' g h)
  let hedge_pred g h = vset_of_array (hedge_pred' g h)

  let list_vertex g = VTbl.fold (fun k _ l -> k::l) g.vertex []
  let list_hedge g = HTbl.fold (fun k _ l -> k::l) g.hedge []

  let vertex_attrib g v = (vertex_n g v).v_attr
  let hedge_attrib g h = (hedge_n g h).h_attr

  let import_hedge g1 g2 h attr =
    let hedge_n = hedge_n g1 h in
    add_hedge g2 h attr ~pred:hedge_n.h_pred ~succ:hedge_n.h_succ

  let import_subgraph g1 g2 vl hl fv fh =
    let import_vertex v = add_vertex g2 v (fv v) in
    let import_hedge h =
      let hedge_n = hedge_n g1 h in
      add_hedge g2 h (fh h) ~pred:hedge_n.h_pred ~succ:hedge_n.h_succ in
    List.iter import_vertex vl;
    List.iter import_hedge hl

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

    let check_member v =
      if not (VTbl.mem in_graph.vertex v)
      then raise (Invalid_argument "clone_subgraph: vertex of sg_input and sg_output are not in in_graph") in
    Array.iter check_member subgraph.sg_input;
    Array.iter check_member subgraph.sg_output;

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
        if not (HSet.subset (hset_of_hiset v_node.v_pred) subgraph.sg_hedge) &&
           not (HSet.subset (hset_of_hiset v_node.v_succ) subgraph.sg_hedge)
        then raise (Invalid_argument "clone_subgraph: not independent subgraph");
        let new_node =
          { v_attr = import_vattr ~new_vertex ~old_attr:v_node.v_attr;
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
          { h_attr = import_hattr ~new_hedge:new_h ~old_attr:h_node.h_attr;
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
            fprintf ppf "%a -> %a;@ "
              print_vertex pred print_hedge hedge
          end)
          hedge_n.h_pred;
        Array.iter
          (begin fun succ ->
            fprintf ppf "%a -> %a;@ "
              print_hedge hedge print_vertex succ
          end)
          hedge_n.h_succ
      end)
      g.hedge
    ;
    fprintf ppf "@]@.}@.";
    ()

end

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

module type Manager = sig

  module H : Hgraph
  open H

  type abstract

  val bottom : T.vertex -> abstract
  val is_bottom : T.vertex -> abstract -> bool
  val is_leq : T.vertex -> abstract -> abstract -> bool
  (* val join : vertex -> abstract -> abstract -> abstract *)
  val join_list : T.vertex -> abstract list -> abstract
  (* val widening : vertex -> abstract -> abstract -> abstract *)
  val abstract_init : T.vertex -> abstract

  type vertex_attribute
  type hedge_attribute
  type graph_attribute
  type function_id

  module Function_id : OrderedHashedType with type t = function_id

  val find_function : function_id ->
    (vertex_attribute, hedge_attribute, graph_attribute) graph * subgraph

  val clone_vertex : T.vertex -> T.vertex
  val clone_hedge : T.hedge -> T.hedge

  val apply : T.hedge -> hedge_attribute -> abstract array ->
    abstract array * function_id list

end

module Fixpoint(Manager:Manager) : sig

  type input_graph = (Manager.vertex_attribute,
                      Manager.hedge_attribute,
                      Manager.graph_attribute)
      Manager.H.graph

  val kleene_fixpoint :
    input_graph -> Manager.H.VertexSet.t ->
    (Manager.abstract, unit, unit) Manager.H.graph

end = struct
  open Manager
  open H
  module VSet = VertexSet
  module VTbl = VertexTbl
  module HSet = HedgeSet
  module HTbl = HedgeTbl
  module M = Manager
  type abstract = M.abstract

  type input_graph = (vertex_attribute, hedge_attribute, graph_attribute) graph

  type output = (abstract,unit,unit) graph

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

  module Vwq = WorkQueue(VertexSet)
  module Hwq = WorkQueue(HedgeSet)

  module HedgeFid = struct
    type t = T.hedge * function_id
    let hash (h,f) =
      Hashtbl.hash (T.Hedge.hash h, Function_id.hash f)
    let compare (h1,f1) (h2,f2) =
      let c = T.Hedge.compare h1 h2 in
      if c <> 0 then c
      else Function_id.compare f1 f2
    let equal (h1,f1) (h2,f2) =
      T.Hedge.equal h1 h2 && Function_id.equal f1 f2
  end
  module HedgeFidSet = Set.Make(HedgeFid)

  let initialise g =
    let import_vertex _ = () in
    let import_hedge v = hedge_attrib g v in
    let all_vertex = list_vertex g in
    let all_hedge = list_hedge g in
    let g' = create ~size:(List.length all_hedge) () in
    import_subgraph g g' all_vertex all_hedge import_vertex import_hedge;
    g'

  let return_graph g vertex_table =
    let import_vertex v =
      try VTbl.find vertex_table v with
      | Not_found -> M.bottom v in
    let import_hedge _ = () in
    let all_vertex = list_vertex g in
    let all_hedge = list_hedge g in
    let g' = create ~size:(List.length all_hedge) () in
    import_subgraph g g' all_vertex all_hedge import_vertex import_hedge;
    g'

  let import_hedge_attr tbl g =
    List.iter (fun h -> HTbl.add tbl h (hedge_attrib g h)) (list_hedge g)

  let kleene_fixpoint orig_g sinit =
    let g = initialise orig_g in

    (* hedge attribute table: contains also attributes
       from imported subgraphs *)
    let h_attrib = HTbl.create 10 in
    import_hedge_attr h_attrib orig_g;

    (* vertex working queue *)
    let vwq = Vwq.create () in

    (* hyper edge working queue *)
    let hwq = Hwq.create () in

    Vwq.push_set vwq sinit;

    let hedge_result : abstract array HTbl.t = HTbl.create 10 in
    let vertex_result = VTbl.create 10 in

    let imported_functions = ref HedgeFidSet.empty in

    let hedge_abstract (i,h) =
      try
        Some (HTbl.find hedge_result h).(i)
      with
      | Not_found -> None
    in

    let vertex_abstract v =
      try VTbl.find vertex_result v with
      | Not_found -> M.bottom v
    in

    let update_vertex v =
      let pred = HedgeIntSet.elements (vertex_pred' g v) in
      let abstract = match pred with
        | [] -> M.abstract_init v
        | _ ->
          match map_filter hedge_abstract pred with
          | [] -> M.bottom v
          | [a] -> a
          | l -> M.join_list v l
      in
      if not (M.is_leq v abstract (vertex_abstract v))
      then begin
        (* Printf.printf "increasing\n%!"; *)
        Hwq.push_set hwq (vertex_succ g v)
      end
      (* else Printf.printf "not increasing\n%!" *)
    ;
      VTbl.replace vertex_result v abstract
    in

    let import_function h f =
      let (in_graph,subgraph) = Manager.find_function f in
      let input = hedge_pred' g h in
      let output = hedge_succ' g h in
      let import_hedge hedge =
        let new_hedge = Manager.clone_hedge hedge in
        HTbl.add h_attrib new_hedge (hedge_attrib in_graph hedge);
        new_hedge
      in
      let subgraph, input_hedges, output_hedges = clone_subgraph
        ~in_graph
        ~out_graph:g
        ~import_vattr:(fun ~new_vertex ~old_attr -> ())
        ~import_hattr:(fun ~new_hedge ~old_attr -> old_attr)
        ~clone_vertex:Manager.clone_vertex
        ~clone_hedge:import_hedge
        ~input
        ~output
        subgraph
      in
      Array.fold_left (fun hset hiset ->
          HedgeIntSet.fold (fun (_,v) set -> HedgeSet.add v set) hiset hset)
        HedgeSet.empty input_hedges
    in
    let import_function h f =
      if not (HedgeFidSet.mem (h,f) !imported_functions)
      then begin
        imported_functions := HedgeFidSet.add (h,f) !imported_functions;
        import_function h f
      end
      else HedgeSet.empty
    in

    let update_hedge h =
      let pred = hedge_pred' g h in
      let f v =
        let abstract = vertex_abstract v in
        if M.is_bottom v abstract
        then None
        else Some abstract
      in
      let pred' = Array.map f pred in
      match lift_option_array pred' with
      | None -> ()
      | Some abstract ->
        let attrib = HTbl.find h_attrib h in
        let results,functions = M.apply h attrib abstract in
        Vwq.push_set vwq (hedge_succ g h);
        List.iter (fun fid -> Hwq.push_set hwq (import_function h fid)) functions;
        (* TODO avoid recursive call infinite expansion *)
        HTbl.replace hedge_result h results
    in

    let rec loop () =
      match Vwq.pop vwq with
      | None ->
        begin match Hwq.pop hwq with
          | None -> ()
          | Some h ->
            update_hedge h;
            loop ()
        end
      | Some v ->
        update_vertex v;
        loop ()
    in
    loop ();
    return_graph g vertex_result

end
