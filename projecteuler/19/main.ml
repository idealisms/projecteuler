let isLeapYear year =
  (year mod 4 = 0 && year mod 100 != 0) or (year mod 400 = 0)
;;

let pastLastDay day month year =
  match month with
    1 | 3 | 5 | 7 | 8 | 10 | 12 -> day = 32
  | 4 | 6 | 9 | 11 -> day = 31
  | 2 -> if isLeapYear year then day = 30 else day = 29
  | _ -> raise Not_found
;;
      
let rec countSundayFirsts dayOfWeek day month year cnt =
  if year == 2001 then
    cnt
  else if month = 13 then
    countSundayFirsts dayOfWeek day 1 (year + 1) cnt
  else if pastLastDay day month year then
    countSundayFirsts dayOfWeek 1 (month + 1) year cnt
  else begin
    if year >= 1901 && day = 1 && dayOfWeek mod 7 = 0 then
      countSundayFirsts (dayOfWeek + 1) (day + 1) month year (cnt + 1)
    else
      countSundayFirsts (dayOfWeek + 1) (day + 1) month year cnt
  end
;;

Printf.printf "%d\n" (countSundayFirsts 1 1 1 1900 0);;
