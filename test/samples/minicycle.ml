type 'a ref = { mutable contents : 'a }
external ref : 'a -> 'a ref = "%makemutable"
external ( ! ) : 'a ref -> 'a = "%field0"
external ( := ) : 'a ref -> 'a -> unit = "%setfield0"
external ( + ) : int -> int -> int = "%addint"

let _ =
  let x = ref 0 in
  while true do
    x := !x + 1
  done
