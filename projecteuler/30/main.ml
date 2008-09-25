(* Since the largest sum from a 7 digit number is 9^5*7 = 413343, the possible
   numbers must be 6 digits or less. *)

#load "nums.cma";;

let pow a b =
  Num.int_of_num (Num.power_num (Num.Int a) (Num.Int b))
;;

let itoa num =
  Printf.sprintf "%d" num
;;

let powers = Array.of_list [(pow 0 5); (pow 1 5); (pow 2 5); (pow 3 5);
    (pow 4 5); (pow 5 5); (pow 6 5); (pow 7 5); (pow 8 5); (pow 9 5)]
;;

let getDigit num pos =
  Char.code (String.get num pos) - Char.code '0'
;;

let rec sumFifthsHelper numStr pos total =
  if pos == String.length numStr then
    total
  else
    sumFifthsHelper numStr (pos + 1) (total + powers.(getDigit numStr pos))
;;

let sumFifths num =
  sumFifthsHelper (itoa num) 0 0
;;

let rec findFifths n total =
  if n = 1000000 then
    total
  else begin
    if n = sumFifths n then begin
      findFifths (n + 1) (total + n)
    end else
      findFifths (n + 1) total
  end
;;

Printf.printf "%d\n" (findFifths 10 0);;
