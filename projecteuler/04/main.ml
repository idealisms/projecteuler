open Printf;;
open String;;

let nextPair (a, b) =
  if 1000 = b then
    a + 1, 100
  else
    a, b + 1
;;

let isPalindrome n =
  let rec check str pos =
    let len = String.length str
    in
      if pos > len / 2 then
        true
      else begin
        if (String.get str pos) = (String.get str (len - pos - 1)) then
          check str (pos + 1)
        else
          false
      end
  in
    check (Printf.sprintf "%d" n) 0
;;

let rec largePalindrome (a, b) largest =
  if 1000 = a then
    largest
  else begin
    if isPalindrome (a * b) then
      largePalindrome (nextPair (a, b)) (max (a * b) largest)
    else
      largePalindrome (nextPair (a, b)) largest
  end
;;
Printf.printf "%d\n" (largePalindrome (100, 100) 0);;
