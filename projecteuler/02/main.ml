open Printf;;

let rec func a b total =
  if b > 4000000 then
    total
  else begin
    if b mod 2 = 0 then
      func b (a + b) (total + b)
    else
      func b (a + b) total
  end
in
  Printf.printf "%d\n" (func 1 1 0);;
