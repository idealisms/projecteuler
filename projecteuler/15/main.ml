open Printf;;
open Hashtbl;;

let mem = Hashtbl.create (21 * 21);;

let rec countRoutes rows cols =
  if rows = 0 or cols = 0 then
    1L
  else begin
    if Hashtbl.mem mem (rows, cols) then
      Hashtbl.find mem (rows, cols)
    else begin
      let routes = Int64.add (countRoutes (rows - 1) cols)
                             (countRoutes rows (cols - 1))
      in
        Hashtbl.add mem (rows, cols) routes;
        routes
    end
  end
;;

Printf.printf "%Ld\n" (countRoutes 20 20);;
