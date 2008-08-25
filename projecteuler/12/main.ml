open Printf;;

let rec countFactors n div factors =
  if div * div = n then
    factors + 1
  else if div * div > n then
    factors
  else begin
    if n mod div = 0 then
      countFactors n (div + 1) (factors + 2)
    else
      countFactors n (div + 1) factors
  end
;;

let rec findFiveHundred n step =
  let factors = (countFactors n 1 0)
  in
    if factors > 500 then
      n
    else
      findFiveHundred (n + step) (step + 1)
;;

Printf.printf "%d\n" (findFiveHundred 1 2);;
