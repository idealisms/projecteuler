let rec factorial n total =
  if n = 0 then
    total
  else
    factorial (n - 1) (total * n)
;;

let rec factorialsList n list =
  if n = 10 then
    list
  else
    factorialsList (n + 1) (List.append list [(factorial n 1)])
;;

let factorials = Array.of_list (factorialsList 0 []);;

let rec factSum n total =
  if n < 10 then
    total + factorials.(n)
  else
    factSum (n / 10) (total + factorials.(n mod 10))
;;

(* 2540160 is the largest 7 digit value, so that's our stopping point. *)
let rec findFactSums n total =
  if n = 2540161 then
    total
  else begin
    let sum = factSum n 0
    in
      if sum = n then begin
        (* Printf.printf "%d\n" n; *)
        findFactSums (n + 1) (total + sum)
      end else
        findFactSums (n + 1) total
  end
;;

Printf.printf "%d\n" (findFactSums 3 0);;
