let rec numLetters num =
  match num with
    1 | 2 | 6 | 10 -> 3
  | 4 | 5 | 9 -> 4
  | 3 | 7 | 8 | 40 | 50 | 60 -> 5
  | 11 | 12 | 20 | 30 |80 | 90 -> 6
  | 15 | 16 | 70 -> 7
  | 13 | 14 | 18 | 19 -> 8
  | 17 -> 9
  | _ ->
    if num < 100 then
      let dec = num / 10 * 10
      in
        (numLetters dec) + numLetters (num - dec)
    else if num < 1000 then
    begin
      let hund = num / 100
      in
        if num mod 100 = 0 then
          (numLetters hund) + 7 (* hundred *)
        else
          (numLetters hund) + 10 (* hundred and *) +
            (numLetters (num - (hund * 100)))
    end
    else if num = 1000 then
      11 (* one thousand *)
    else
      raise Not_found
;;

let rec countLetters num total =
  if num = 1001 then
    total
  else
    countLetters (num + 1) (total + numLetters num)
;;

Printf.printf "%d\n" (countLetters 1 0);;
