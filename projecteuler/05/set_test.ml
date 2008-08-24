open Set;;

module IntSet = Set.Make (struct
                            type t = int
                            let compare = compare
                          end);;
IntSet;;
(*
let intset = IntSet;;
intset#size;;
*)