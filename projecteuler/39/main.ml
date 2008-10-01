let triples = Hashtbl.create 500;;

let rec findPythagTripleC a b c =
  if a + b + c >= 1000 then
    ()
  else begin
    if a * a + b * b = c * c then begin
      Printf.printf "%d, %d, %d\n" a b c;
      Hashtbl.add triples (a + b + c) (a, b, c)
    end;
    findPythagTripleC a b (c + 1)
  end
;;

let rec findPythagTripleB a b =
  if a + b + b >= 1000 then
    ()
  else begin
    findPythagTripleC a b b;
    findPythagTripleB a (b + 1)
  end
;;

let rec findPythagTriple a =
  if a > 333 then
    ()
  else begin
    findPythagTripleB a a;
    findPythagTriple (a + 1)
  end
;;

findPythagTriple 1;;

let totals = Array.make 1000 0;;

Hashtbl.iter (fun key value -> totals.(key) <- totals.(key) + 1) triples;;

let rec findMax pos bestTotal bestPos =
  if pos = Array.length totals then
    bestPos, bestTotal
  else begin
    if totals.(pos) > bestTotal then
      findMax (pos + 1) totals.(pos) pos
    else
      findMax (pos + 1) bestTotal bestPos
  end
;;

let ans, cnt = findMax 0 0 0
in
  Printf.printf "%d (%d triples)\n" ans cnt;;
