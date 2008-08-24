open Printf;;
open Int64;;

let rec func divisor n largest =
  if (Int64.mul divisor divisor) > n then
    max largest n
  else begin
    if (Int64.rem n divisor) = 0L then
      func divisor (Int64.div n divisor) divisor
    else
      func (Int64.add divisor 1L) n largest
  end
in
  Printf.printf "%Ld\n" (func 2L 600851475143L 0L);;
