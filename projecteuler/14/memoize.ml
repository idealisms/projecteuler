(*
  I thought I was going to need to memoize values while computing Collatz
  sequences, but it turns out that most of the sequences are short and
  the memoization was just taking more time.
  
  I'm just keeping this here in case I need this later.
*)

open Map;;

module IntMap = Map.Make(Int32);;

let mem = ref IntMap.empty;;

let addMem k v =
  mem := IntMap.add (Int32.of_int k) v !mem
;;

let getMem k =
  try
    IntMap.find (Int32.of_int k) !mem
  with
    Not_found -> -1
;;
