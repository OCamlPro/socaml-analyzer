open Common_types
open Data

(* Environment management *)

let is_bottom = function
  | Bottom -> true
  | _ -> false
 
let bottom = Bottom
let empty = Env Idm.empty


(* Environment joining *)

let join e1 e2 =
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

let is_leq e1 e2 =
  match e1, e2 with
  | Bottom, _ -> true
  | _, Bottom -> false
  | Env e1, Env e2 ->
    Idm.for_all (fun id d -> try is_leq d ( Idm.find id e2) with Not_found -> false ) e1



(* Garbage collection *)

let gc roots env =
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
   | App_prep ( x, y )
   | Send ( x, y ) -> aux ( x :: y :: res ) tl
   | Constraint _
   | Const _ -> aux res tl
   | Prim ( _, l )
   | Ccall ( _, l )->
     aux ( List.rev_append l res ) tl
   | Return _ | Retexn _ -> failwith "TODO: GC function return"
   | App -> assert false
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
  | Env m -> aux empty m roots
    
