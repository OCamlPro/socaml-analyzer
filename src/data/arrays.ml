open Common_types
open Data

let singleton l size akind =
  let open Lambda in
  {
    bottom with
    arrays =
      {
        a_elems = List.fold_left (fun e i -> Ids.add i e) Ids.empty l;
        a_size = size;
        a_gen = akind = Pgenarray;
        a_float = akind = Pfloatarray;
        a_addr = akind = Paddrarray;
        a_int = akind = Pintarray;
      }
  }

let restrict x = { bottom with arrays = x }

let restrict_not x = { x with arrays = bottom.arrays }

let add_field x i =
  { x with
    arrays =
      { x.arrays with
        a_elems = Ids.add i x.arrays.a_elems
      }
  }

let size x = x.arrays.a_size

let get x = x.arrays.a_elems

let set x id =
  { x with
    arrays =
      { x.arrays with
        a_elems = Ids.add id x.arrays.a_elems;
      }
  }
