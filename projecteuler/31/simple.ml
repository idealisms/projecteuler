(* The recursive logic for this problem expressed a bit more clearly
   than in main.ml (but with a lot more code). *)

(* Using only 1p coins, how many ways can you make target? *)
let countWaysOne target =
  1
;;

(* Using 1p and 2p coins, how many ways can you make target? *)
let rec countWaysTwo target total =
  if target < 0 then
    total
  else begin
    countWaysTwo (target - 2) (total + (countWaysOne target))
  end
;;

(* Using 1p, 2p, 5p coins, how many ways can you make target? *)
let rec countWaysFive target total =
  if target < 0 then
    total
  else begin
    countWaysFive (target - 5) (total + (countWaysTwo target 0))
  end
;;

(* Using 1p, 2p, 5p, 10p coins, how many ways can you make target? *)
let rec countWaysTen target total =
  if target < 0 then
    total
  else begin
    countWaysTen (target - 10) (total + (countWaysFive target 0))
  end
;;

(* Using 1p, 2p, 5p, 10p, 20p coins, how many ways can you make target? *)
let rec countWaysTwenty target total =
  if target < 0 then
    total
  else begin
    countWaysTwenty (target - 20) (total + (countWaysTen target 0))
  end
;;

(* Using 1p, 2p, 5p, 10p, 20p, 50p coins, how many ways can you make target? *)
let rec countWaysFifty target total =
  if target < 0 then
    total
  else begin
    countWaysFifty (target - 50) (total + (countWaysTwenty target 0))
  end
;;

(* Using 1p, 2p, 5p, 10p, 20p, 50p, 100p coins, how many ways can you make
   target? *)
let rec countWaysOneHundred target total =
  if target < 0 then
    total
  else begin
    countWaysOneHundred (target - 100) (total + (countWaysFifty target 0))
  end
;;

(* Using 1p, 2p, 5p, 10p, 20p, 50p, 100p, 200p coins, how many ways can you
   make target? *)
let rec countWaysTwoHundred target total =
  if target < 0 then
    total
  else begin
    countWaysTwoHundred (target - 200) (total + (countWaysOneHundred target 0))
  end
;;


Printf.printf "%d\n" (countWaysTwoHundred 200 0);;
