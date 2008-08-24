open Printf;;

let rec pythag =
  for a = 1 to 334 do
    for b = (a + 1) to 500 do
      let c = 1000 - a -b
      in
        if c > b && (a * a + b * b = c * c) then begin
          Printf.printf "%d * %d * %d = %d\n" a b c (a * b * c)
        end
    done
  done
;;

pythag;;
