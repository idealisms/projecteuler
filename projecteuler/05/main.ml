open Printf;;
open List;;

let rec gcd a b =
  if b = 0 then
    a
  else
    gcd b (a mod b)
;;

let lcm a b =
  a * b / (gcd a b);;

let rec makeList n stop lst =
  if n = stop then
    lst
  else
    makeList (n + 1) stop (List.rev_append lst [n])
;;

let ans = List.fold_left lcm 1 (makeList 1 20 [])
in Printf.printf "%d\n" ans;;

