open Printf;;

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

let rec findPairs n total =
  if n = 10000 then
    total
  else begin
    let d = sumFactors n 1 0
    in
      if d < 10000 && (sumFactors d 1 0) = n then
        findPairs (n + 1) (total + n)
      else
        findPairs (n + 1) total
  end
;;

Printf.printf "%d\n" (findPairs 1 0);;
