(* First define a function to do long division and collect the factors in a
   list.  This method raises No_recurrence if the division terminates
   cleanly. *)
exception No_recurrence;;

let rec longDivisionHelper cur n factors =
  if List.exists (fun x -> x = cur) factors then
    cur::factors
  else if cur = 0 then
    raise No_recurrence
  else begin
    if n <= cur then
      longDivisionHelper (10 * (cur mod n)) n (cur::factors)
    else
      longDivisionHelper (10 * cur) n (0::factors)
  end
;;

let longDivision n =
  longDivisionHelper 10 n []
;;

(* Now make a helper method that scans the factors list to determine the
   length of the recurrence. *)
let rec findFactors key factors n =
  match factors with
    hd::tl ->
      if key = hd then
        n + 1
      else
        findFactors key tl (n + 1)
  | _ -> raise Not_found
;;

let recurrenceLength factors =
  match factors with
    hd::tl -> findFactors hd tl 0
  | _ -> raise Not_found
;;

(* Finally, find the longest recurrence from 1/2 .. 1/999. *)
let rec findLongestRecurrence n longest longestN =
  if n = 1000 then
    longestN
  else begin
    try
      let factors = longDivision n
      in
        let recLen = recurrenceLength factors
        in
          if recLen > longest then
            findLongestRecurrence (n + 1) recLen n
          else
            findLongestRecurrence (n + 1) longest longestN
    with
      No_recurrence -> findLongestRecurrence (n + 1) longest longestN
  end
;;

Printf.printf "%d\n" (findLongestRecurrence 2 0 0);;
