open Common_types
open Data

let singleton tag content =
  { bottom with blocks = Tagm.singleton tag ( Intm.singleton ( Array.length content) ( Array.map Ids.singleton content) ) }

let restrict ?tag ?has_field ?size d =
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

let fieldn_map f n b =
  { b with
    blocks =
      Tagm.mapi
        (fun t sizes ->
           Intm.mapi
             (fun s a ->
                let a' = Array.copy a in
                a'.(n) <-
                  Ids.fold
                    (fun e -> Ids.add (f t s e))
                    a.(n) Ids.empty;
                a'
             ) sizes
        ) b.blocks;
  }

let has_tag t d = Tagm.mem t d.blocks
let is_one_tag d env =
  Tagm.cardinal d.blocks = 1 &&
  is_bottom env { d with blocks = bottom.blocks }


let set_field i v b =
  let b = restrict ~has_field:i b in
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

let sizes ~tag { blocks; _ } =
  let a = Tagm.find tag blocks in
  Intm.fold (fun s _ i -> Int.join (Int.singleton s) i) a bottom
