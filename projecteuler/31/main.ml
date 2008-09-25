(* For a clearer explanation of this problem, see simple.ml. *)

let values = Array.of_list [200; 100; 50; 20; 10; 5; 2];;

(* target is the value we're trying to get, n is the position in the values
   array. *)
let rec countWaysN target n total =
  if n = 7 then
    1
  else if target < 0 then
    total
  else
    countWaysN (target - values.(n)) n (total + (countWaysN target (n + 1) 0))
;;

Printf.printf "%d\n" (countWaysN 200 0 0);;
