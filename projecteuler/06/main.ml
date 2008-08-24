open Printf;;

let rec sumOfSquares n total =
  if n = 101 then
    total
  else
    sumOfSquares (n + 1) (total + (n * n))
;;

let rec squareOfSum n total =
  if n = 101 then
    total * total
  else
    squareOfSum (n + 1) (total + n)
;;

let a = (squareOfSum 1 0)
in
  let b = (sumOfSquares 1 0)
    in
      Printf.printf "%d - %d = %d\n" a b (a - b)
;;
