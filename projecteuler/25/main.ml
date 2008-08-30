#load "nums.cma";;

open Big_int;;

let rec fib a b n =
  let s = (Big_int.string_of_big_int b)
  in
    if String.length s = 1000 then
      n, s
    else
      fib b (Big_int.add_big_int a b) (n + 1)
;;

let n, value = (fib Big_int.unit_big_int Big_int.unit_big_int 2);;
Printf.printf "%d %s\n" n value;;
