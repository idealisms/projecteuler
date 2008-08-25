open Int64;;
open Printf;;

let rec collatzLen n =
  if n < 0L then
    Printf.printf "%Ld\n" n;

  if n = 1L then
    1
  else begin
    if (Int64.rem n 2L) = 0L then
      1 + (collatzLen (Int64.div n 2L))
    else
      1 + (collatzLen (Int64.add (Int64.mul 3L n) 1L))
  end
;;

let rec longCollatz n longest_start longest_len =
  if n = 1000000 then
    (longest_start, longest_len)
  else begin
    let len = collatzLen (Int64.of_int n)
    in
      if len > longest_len then
        longCollatz (n + 1) n len
      else
        longCollatz (n + 1) longest_start longest_len
  end
;;

let ans, len = (longCollatz 2 1 1)
in
  Printf.printf "%d (%d)\n" ans len;;
