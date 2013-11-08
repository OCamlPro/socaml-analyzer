open Data

let singleton s = { bottom with float = Constants.singleton s; }

let array l = { bottom with floata = Constants.singleton ( String.concat ";" l ) ; }
