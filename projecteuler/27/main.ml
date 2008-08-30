
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

let rec computeNumPrimesHelper a b n =
  if isPrime (n * n + a * n + b) then
    computeNumPrimesHelper a b (n + 1)
  else
    n
;;

let computeNumPrimes a b =
  computeNumPrimesHelper a b 0
;;

let rec solve a b bestNumPrimes best =
  if a = 1000 then
    best
  else if b = 1000 then
    solve (a + 1) (-999) bestNumPrimes best
  else begin
    let numPrimes = computeNumPrimes a b
    in
      if numPrimes > bestNumPrimes then
        solve a (b + 1) numPrimes (a, b)
      else
        solve a (b + 1) bestNumPrimes best
  end
;;

let bestA, bestB = solve (-999) (-999) 0 (0, 0);;
Printf.printf "%d * %d = %d\n" bestA bestB (bestA * bestB);;
