open Printf;;

let rec func n total =
  if n = 1000 then total
  else begin
    if (n mod 3 = 0) or (n mod 5 = 0) then
      func (n + 1) (total + n)
    else
      func (n + 1) total
  end
in
  Printf.printf "%d\n" (func 1 0);;
