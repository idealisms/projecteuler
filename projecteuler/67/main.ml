#load "str.cma";;

open Hashtbl;;
open List;;
open Printf;;
open Scanf;;
open Str;;

let filename = "triangle.txt";;
let chan = open_in filename;;

let strToInt s =
  Scanf.sscanf s "%d" (fun x -> x);;

let rec makeTriangle triangle =
  try
    let line = input_line chan
    in
      makeTriangle (triangle @
        [ List.map strToInt (Str.split (Str.regexp " ") line) ]
      )
  with End_of_file -> triangle
;;

let triangle = makeTriangle [];;

let mem = Hashtbl.create ((100 * 101) / 2);;

let rec maxRoute row offset =
  let cur = List.nth (List.nth triangle row) offset
  in
    if Hashtbl.mem mem (row, offset) then
      Hashtbl.find mem (row, offset)
    else begin
      if row = (List.length triangle) - 1 then
        cur
      else
        let best = max (cur + (maxRoute (row + 1) offset))
                       (cur + (maxRoute (row + 1) (offset + 1)))
        in
          Hashtbl.add mem (row, offset) best;
          best
    end
;;

Printf.printf "%d\n" (maxRoute 0 0);;
