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
  module HedgeSet : (Set.S with type t = Set.Make(T.Hedge).t and type elt=T.hedge)
  (** Set module for hyperedges *)
  module VertexTbl : (Hashtbl.S with type 'a t = 'a Hashtbl.Make(T.Vertex).t and type key=T.vertex)
  (** Hash module with vertices as keys *)
  module HedgeTbl : (Hashtbl.S with type 'a t = 'a Hashtbl.Make(T.Hedge).t and type key=T.hedge)
  (** Hash module with hyperedges as keys *)

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
    import_vattr:('a -> 'd) ->
    import_hattr:('b -> 'e) ->
    clone_vertex:(T.vertex -> T.vertex) ->
    clone_hedge:(T.hedge -> T.hedge) ->
    input:T.vertex array ->
    output:T.vertex array ->
    subgraph -> subgraph
  (** [clone_subgraph] returns a copy of the subgraph by copying the
      vertices and edges in out_graph. The vertex sg_input and
      sg_output are not copied, but are replaced by the provided input
      and output. There should be no link outside of the subgraph
      except in sg_input and sg_output. There must be no vertex from
      sg_input or sg_output in sg_vertex *)

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
end

module Make(T:T) : Hgraph with module T = T = struct
  module T = T
  open T
  module VertexSet = Set.Make(T.Vertex)
  module HedgeSet = Set.Make(T.Hedge)
  module VertexTbl = Hashtbl.Make(T.Vertex)
  module HedgeTbl = Hashtbl.Make(T.Hedge)

  module VSet = VertexSet
  module VTbl = VertexTbl
  module HSet = HedgeSet
  module HTbl = HedgeTbl

  let vset_of_array a = Array.fold_right VSet.add a VSet.empty
  let hset_of_array a = Array.fold_right HSet.add a HSet.empty

  (* module T = T *)

  type 'vattr vertex_n = {
    v_attr : 'vattr;
    mutable v_pred : HSet.t;
    mutable v_succ : HSet.t;
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
    add_vertex_node g v { v_attr; v_pred = HSet.empty; v_succ = HSet.empty }

  let add_hedge g h h_attr ~pred ~succ =
    if contains_hedge g h
    then failwith "add_hedge: the hedge is already in the graph";
    begin try
      Array.iter
        (fun v ->
          let vertex_n =  vertex_n g v in
          vertex_n.v_succ <- HSet.add h vertex_n.v_succ)
        pred;
      Array.iter
        (fun v ->
          let vertex_n =  vertex_n g v in
          vertex_n.v_pred <- HSet.add h vertex_n.v_pred)
        succ;
    with Not_found ->
      failwith "add_hedge: origin or destination vertex does not exists"
    end;
    HTbl.add g.hedge h
      { h_attr; h_pred = pred; h_succ = succ }

  let vertex_succ g v = (vertex_n g v).v_succ
  let vertex_pred g v = (vertex_n g v).v_pred
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

    let add_matching_hedge h set = HSet.add (HTbl.find hedge_mapping h) set in
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
        if not (HSet.subset v_node.v_pred subgraph.sg_hedge) &&
           not (HSet.subset v_node.v_succ subgraph.sg_hedge)
        then raise (Invalid_argument "clone_subgraph: not independent subgraph");
        let new_node =
          { v_attr = import_vattr v_node.v_attr;
            v_pred = HSet.fold add_matching_hedge v_node.v_pred HSet.empty;
            v_succ = HSet.fold add_matching_hedge v_node.v_succ HSet.empty } in
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
          { h_attr = import_hattr h_node.h_attr;
            h_pred = Array.map matching_vertex h_node.h_pred;
            h_succ = Array.map matching_vertex h_node.h_succ } in
        add_hedge_node out_graph new_h new_node) assoc_hedge_list;

    { sg_input  = Array.map matching_vertex subgraph.sg_input;
      sg_output = Array.map matching_vertex subgraph.sg_output;
      sg_vertex = imported_vertex;
      sg_hedge  = imported_hedge }

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
  type hedge_attribute
  type function_id

  val bottom : T.vertex -> abstract
  val is_bottom : T.vertex -> abstract -> bool
  val is_leq : T.vertex -> abstract -> abstract -> bool
  (* val join : vertex -> abstract -> abstract -> abstract *)
  val join_list : T.vertex -> abstract list -> abstract
  (* val widening : vertex -> abstract -> abstract -> abstract *)
  val abstract_init : T.vertex -> abstract

  val find_function : ('a,'b,'c) graph -> function_id -> ('a,'b,'c) graph * subgraph

  val apply : T.hedge -> hedge_attribute -> abstract array ->
    abstract array * function_id list

end

module Fixpoint(Manager:Manager) = struct
  open Manager
  open H
  module VSet = VertexSet
  module VTbl = VertexTbl
  module HSet = HedgeSet
  module HTbl = HedgeTbl
  module M = Manager
  type abstract = M.abstract

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

  let kleene_fixpoint orig_g sinit =
    let g = initialise orig_g in

    (* vertex working queue *)
    let vwq = Vwq.create () in

    (* hyper edge working queue *)
    let hwq = Hwq.create () in

    Vwq.push_set vwq sinit;

    let hedge_result : abstract array HTbl.t = HTbl.create 10 in
    let vertex_result = VTbl.create 10 in

    let hedge_abstract v =
      try
        let a = HTbl.find hedge_result v in
        if Array.length a > 1 then failwith "TODO multiple output";
        Some a.(0)
      with
      | Not_found -> None
    in

    let vertex_abstract v =
      try VTbl.find vertex_result v with
      | Not_found -> M.bottom v
    in

    let update_vertex v =
      let pred = HSet.elements (vertex_pred g v) in
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
        let attrib = hedge_attrib orig_g h in
        let results,functions = M.apply h attrib abstract in
        Vwq.push_set vwq (hedge_succ g h);

        (* List.iter (fun (entry,_) -> *)
        (*   Hwq.push_set hwq (HedgeSet.singleton entry)) links; *)

        if functions <> [] then failwith "TODO functions";

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
