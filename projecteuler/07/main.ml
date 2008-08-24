open Printf;;

(* |divisor| should be 2 in the initial call *)
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

let rec findPrime n prime_count nth_prime =
  if prime_count = 10001 then
    nth_prime
  else begin
    if isPrime n 2 then
      findPrime (n + 1) (prime_count + 1) n
    else
      findPrime (n + 1) prime_count nth_prime
  end
;;

Printf.printf "%d\n" (findPrime 2 0 2);;
