open Printf;;

(* From bc:
  define f (x) {
    if (x <= 1) return (1);
    return (f(x-1) * x);
  }
  f(100)
*)
let num =
"93326215443944152681699238856266700490715968264381621468592963895217" ^
"59999322991560894146397615651828625369792082722375825118521091686400" ^
"0000000000000000000000";;

let rec sumDigits pos total =
  if pos = String.length num then
    total
  else
    sumDigits (pos + 1)
              (total + (Char.code (String.get num pos) - Char.code '0'))
;;

Printf.printf "%d\n" (sumDigits 0 0);;
