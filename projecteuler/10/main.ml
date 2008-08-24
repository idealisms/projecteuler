open Int64;;
open Printf;;

let rec isPrime n divisor =
  if divisor * divisor > n then
    true
  else begin
    if n mod divisor = 0 then
      false
    else
      isPrime n (divisor + 1)
  end
;;

(* surprisingly, the sum of primes overflows int32 *)
let rec sumPrimes n total =
  if n = 2000001 then
    total
  else begin
    if (isPrime n 2) then begin
      (*
      Printf.printf "%d\n" n;
      *)
      sumPrimes (n + 1) (Int64.add total (Int64.of_int n))
    end
    else
      sumPrimes (n + 1) total
  end
;;

Printf.printf "%Ld\n" (sumPrimes 2 0L);;
