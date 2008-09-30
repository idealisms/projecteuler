let itoa num =
  Printf.sprintf "%d" num
;;

let rec itoba num s =
  if num = 0 then
    s
  else
    if (num mod 2) = 0 then
      itoba (num / 2) (s ^ "0")
    else
      itoba (num / 2) (s ^ "1")
;;

let rec isStrPalindrome str pos =
  let len = String.length str
  in
    if pos > len / 2 then
      true
    else begin
      if str.[pos] = str.[len - pos - 1] then
        isStrPalindrome str (pos + 1)
      else
        false
    end
;;

let isPalindrome n =
  isStrPalindrome (itoa n) 0
;;

let isBinaryPalindrome n =
  isStrPalindrome (itoba n "") 0
;;

let rec findPalindromes n total =
  if n > 1000000 then
    total
  else begin
    (* We only test odd numbers because the binary representation can't be
       a palindrome since it ends with a 0. *)
    if (isPalindrome n) && (isBinaryPalindrome n) then
      findPalindromes (n + 2) (total + n)
    else
      findPalindromes (n + 2) total
  end
;;

Printf.printf "%d\n" (findPalindromes 1 0);;
