(* let's be a little naughty here *)

exception E
exception X
exception N

let x = 1

let y = x / 2

let _ =
  if (try
        y / 2 > 0
      with _ -> raise E )
  then raise X
  else raise N
