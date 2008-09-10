#load "nums.cma";;
open Hashtbl;;

let mem = Hashtbl.create (100 * 100);;

let rec computePowers a b =
  if a = 101 then
    ()
  else if b = 101 then
    computePowers (a + 1) 2
  else begin
    let big_val = Big_int.power_int_positive_int a b
    in
      Hashtbl.replace mem (Big_int.string_of_big_int big_val) true;
      computePowers a (b + 1);
  end
;;

computePowers 2 2;;
Printf.printf "%d\n" (Hashtbl.length mem);;
