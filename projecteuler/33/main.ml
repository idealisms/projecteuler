let itoa num =
  Printf.sprintf "%d" num
;;

let atoi str =
  Scanf.sscanf str "%d" (fun x -> x)
;;

let isCurious numerator denominator =
  let numStr = itoa numerator
  in
    let denStr = itoa denominator
    in
      if (String.get numStr 0) = (String.get denStr 1) &&
         (numerator * (atoi (String.sub denStr 0 1)) =
          denominator * (atoi (String.sub numStr 1 1))) then
        true
      else if (String.get numStr 1) = (String.get denStr 0) &&
              (numerator * (atoi (String.sub denStr 1 1)) =
               denominator * (atoi (String.sub numStr 0 1))) then
        true
      else
        false
;;

let rec findCurious numerator denominator num_total den_total =
  if numerator = 100 then
    (num_total, den_total)
  else if numerator >= denominator then
    findCurious numerator (denominator + 1) num_total den_total
  else if denominator = 100 then
    findCurious (numerator + 1) 11 num_total den_total
  else if isCurious numerator denominator then begin
    Printf.printf "%d/%d\n" numerator denominator;
    findCurious numerator (denominator + 1) (num_total * numerator) (den_total * denominator)
  end else
    findCurious numerator (denominator + 1) num_total den_total
;;

let rec gcd a b =
  if b = 0 then
    a
  else
    gcd b (a mod b)
;;

let num, den = findCurious 11 11 1 1
in
  Printf.printf "%d / %d\n" num den;
  let ans = den / (gcd num den)
  in
    Printf.printf "%d / %d\n" (num / (gcd num den)) ans;
    Printf.printf "%d\n" ans;;
