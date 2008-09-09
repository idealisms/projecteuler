open Printf;;
open String;;

let stringToPermute = "0123456789";;
let stringLen = String.length stringToPermute;;

(* Scanning right to left, find the first time where s.[n] < s.[n+1] *)
let rec findDec s pos =
  if s.[pos] < s.[pos + 1] then
    pos
  else
    findDec s (pos - 1)
;;  

(* Find the smallest char to the right of s.[n-1] that is greater than
   s.[n]. *)
let rec findMinPos s n minPos minChar refChar =
  if String.length s = n then
    minPos
  else begin
    if s.[n] < minChar && s.[n] > refChar then
      findMinPos s (n + 1) n s.[n] refChar
    else
      findMinPos s (n + 1) minPos minChar refChar
  end
;;

(* Swap s.[pos] with the smallest char to the right of s.[pos] that is
   greater than s.[pos] *)
let swapMax s pos =
  let minPos = findMinPos s (pos + 1) (pos + 1) s.[pos + 1] s.[pos]
  in
    let tmp = s.[pos]
    in
      s.[pos] <- s.[minPos];
      s.[minPos] <- tmp;
      s
;;

let stringToArray s arr =
  for i = 0 to (String.length s) - 1 do
    arr.(i) <- s.[i]
  done
;;

let arrayToString arr s =
  for i = 0 to (String.length s) - 1 do
    s.[i] <- arr.(i)
  done;
  s
;;

(* Sort a string lexigraphically.  This is kind of slow, it first converts
   to an array, sorts the array, then converts back to a string *)
let sortString s =
  let len = String.length s
  in
    if len == 1 then
      s
    else begin
      let arr = Array.make (len) 'a'
      in
        stringToArray s arr;
        Array.sort Pervasives.compare arr;
        let sorted_str = String.create (len)
        in
          arrayToString arr sorted_str
    end
;;

(* This does most of the work.  Given the first descending character position,
   we increment the character and sort the remaining values to the right. *)
let makeNewString s pos =
  (* Printf.printf "makeNewString %s %d\n" s pos; *)
  let newS = swapMax s pos
  in
    String.sub newS 0 (pos + 1) ^
        (sortString (String.sub newS (pos + 1) (stringLen - pos - 1)))
;;

let rec permute s n =
  if n == 1000000 then
    s
  else begin
    (* We permute characters to the right of the first descending char. *)
    let pos = findDec s (stringLen - 2)
    in
      let newS = makeNewString s pos
      in
        permute newS (n + 1)
  end
;;

Printf.printf "%s\n" (permute stringToPermute 1);;
