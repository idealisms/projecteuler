(* |divisor| should be 2 in the initial call *)
let rec isPrimeHelper n divisor =
  if divisor * divisor > n then
    true
  else begin
    if n mod divisor = 0 then
      false
    else
      isPrimeHelper n (divisor + 1)
  end
;;

let isPrime n =
  isPrimeHelper (abs n) 2
;;

(* Computed the primes in advance and keep them in a hash table. *)
let primes = Hashtbl.create 100000;;

let rec computePrimes n =
  if n = 1000000 then
    ()
  else begin
    if isPrime n then begin
      Hashtbl.replace primes (Printf.sprintf "%d" n) true;
      computePrimes (n + 1)
    end else
      computePrimes (n + 1)
  end
;;

computePrimes 2;;
Printf.printf "Found primes, checking for circular primes...\n";;

let rotateStr s =
  (String.sub s 1 ((String.length s) - 1)) ^ (String.sub s 0 1)
;;

let rec isCircularHelper num rotation =
  if rotation = 0 then
    true
  else begin
    if Hashtbl.mem primes num then
      isCircularHelper (rotateStr num) (rotation - 1)
    else
      false
  end
;;

let isCircularPrime num =
  isCircularHelper (rotateStr num) (String.length num - 1)
;;

let checkIsCircular num _ cnt =
  if isCircularPrime num then
    cnt + 1
  else
    cnt
;;

Printf.printf "%d\n" (Hashtbl.fold checkIsCircular primes 0);;
