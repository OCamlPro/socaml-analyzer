open Common_types
open Data

let singleton id size =
  {
    bottom with
    arrays =
      {
        a_elems = Ids.singleton id;
        a_size = size;
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
