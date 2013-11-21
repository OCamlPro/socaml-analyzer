open Data

let field i f env =
  let l = Fm.fold (fun _ a l -> a.(i)::l) f.f [] in
  match l with
  | [x] when Ids.cardinal x = 1 -> get_env ( Ids.choose x ) env
  | _ ->
    List.fold_left (fun data ids ->
        Ids.fold
          (fun i data -> union data ( get_env i env ))
          ids data
      ) bottom  l
      
let fid i fu =
  { bottom with f = Fm.singleton i ( Fm.find i fu.f ); }

let mk i l =
  { bottom with
    f = Fm.singleton i
        ( Array.map
            Ids.singleton
            ( Array.of_list l )
        );
  }

let extract_ids { f; _ } = 
  Fm.fold (fun k _ l -> k::l) f []
