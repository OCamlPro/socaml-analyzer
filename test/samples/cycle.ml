let _ =
  let x = ref 0 in
  (* for i = 1 to 1_000 do *)
  (*   x := !x+1 *)
  (* done; *)
  (* for i = 1 to 1000 do *)
  (*   x := !x+1 *)
  (* done; *)
  (* for i = 1 to 1000 do *)
  (*   x := !x+1 *)
  (* done; *)
  for i = 1 to 0 do
    x := !x+1
  done;
  ()
