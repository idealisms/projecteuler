open Printf;;

(* From bc: 2^1000 *)
let num =
"10715086071862673209484250490600018105614048117055336074437503883703" ^
"51051124936122493198378815695858127594672917553146825187145285692314" ^
"04359845775746985748039345677748242309854210746050623711418779541821" ^
"53046474983581941267398767559165543946077062914571196477686542167660" ^
"429831652624386837205668069376";;

let rec sumDigits pos total =
  if pos = String.length num then
    total
  else
    sumDigits (pos + 1)
              (total + (Char.code (String.get num pos) - Char.code '0'))
;;

Printf.printf "%d\n" (sumDigits 0 0);;
