let rec sumFactors n div total =
  if div * div > n then
    total - n
  else if div * div = n then
    (total + div) - n
  else begin
    if n mod div = 0 then
      sumFactors n (div + 1) (total + div + (n / div))
    else
      sumFactors n (div + 1) total
  end
;;

let isAbundant n =
  (sumFactors n 1 0) > n
;;

(* Computes all the abundant numbers under 28123. *)
let rec findAbundant n lst =
  if n > 28123 then
    lst
  else if isAbundant n then
    findAbundant (n + 1) (n :: lst)
  else
    findAbundant (n + 1) lst
;;

let abundant = List.rev (findAbundant 2 []);;
let numAbundant = List.length abundant;;

(* Use dynamic programming to find pairs. *)
let maxValue = 28125;;
let mem = Array.make maxValue false;;

(* List.nth takes O(n) time so use List.iter instead. *)
let abundantPairsOuter a =
  let sumPairs a b =
    if a + b < maxValue then
      Array.set mem (a + b) true
  in
    List.iter (sumPairs a) abundant
;;

List.iter abundantPairsOuter abundant;;

let rec sumNonAbundant n total =
  if n = maxValue then
    total
  else begin
    if Array.get mem n then begin
      sumNonAbundant (n + 1) total
    end
    else
      sumNonAbundant (n + 1) (total + n)
  end
;;

Printf.printf "%d\n" (sumNonAbundant 0 0);;
