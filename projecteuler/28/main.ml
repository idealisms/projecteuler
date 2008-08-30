open List;;
open Pervasives;;
open Int64;;

(* Compute the diagonal going up and to the right. These are just every
   other square. *)
let rec squareList n lst =
  if n = 502 then
    lst
  else
    let sq = n * 2 - 1
    in
      squareList (n + 1) (List.rev_append [sq * sq] lst)
;;

let squares = List.rev (squareList 1 []);;

(* To compute the other diagonals, start with a value on the upper right
   diagonal and subtract out an even constant. *)
let rec solve lst n fullList =
  if lst = [] then
    fullList
  else begin
    let head = (List.hd lst)
    in
      let newList = List.rev_append fullList [head;
                                              head - n;
                                              head - (2 * n);
                                              head - (3 * n)]
      in
        solve (List.tl lst) (n + 2) newList
  end
;;

let allDiagonals = List.sort Pervasives.compare (solve (List.tl squares) 2 [1]);;

(* Finally, add the list together.  Use Int64 just in case of overflow (it
   turns out this optimization isn't needed and we could just fold_left). *)
let rec sumList64 lst total =
  if lst = [] then
    total
  else
    sumList64 (List.tl lst) (Int64.add (Int64.of_int (List.hd lst)) total)
;;

let total = sumList64 allDiagonals 0L;;

Printf.printf "%Ld\n" total;;
