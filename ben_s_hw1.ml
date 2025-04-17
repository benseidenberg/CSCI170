(* q1*)
let is_after month1 day1 year1 month2 day2 year2 = 
  if year1 > year2 then true (* if the year is before then true*)
  else 
    if month1 > month2 && year1 = year2 then true (* if year is equal to, then check if month is before/after *)
    else 
      if day1 > day2 && month1 = month2 && year1 = year2 then true (*if month+year is ==, check if day is before/after*)
      else false (*if all checks pass, the date is before*);;

(* tests *)
let() = assert (is_after 9 1 2004 9 1 2004 = false);;
let() = assert (is_after 4 16 2004 7 12 2004 = false);;
let() = assert (is_after 10 1 2003 7 15 2003 = true);;
let() = assert (is_after 1 1 2005 12 31 2004 = true);;

(* q2 *)
let days_per = [31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30];; (* list of days per month in a year*)
let rec days_left month day = 
  if month = 12 then 31-day  (* If it's december, add 31 minus the day num *)
  else List.nth days_per (month-1) + days_left (month+1) day;; (* else, take days in month and add the next month until hit December *)

(* tests *)
let() = assert (days_left 12 31 = 0);;
let() = assert (days_left 1 1 = 364);;
let() = assert (days_left 2 25 = 309);;

(* q3 *)
let is_prime num = (* not a good time complexity, but it works so... *)
  match num with 
  | 0 -> false (* edge cases *)
  | 1 -> false 
  |_ -> (* if not 0 or 1 *)
  let rec checkNext next = 
    if next >= num then true (* if the divisor is greater or equal to num (becomes final case) *)
    else if num mod next = 0 then false (* if num divided by next is an int then it's not prime *)
    else checkNext (next+1) (* else check the next number up *)
  in checkNext 2 (* start*);;

(* tests *)
let() = assert (is_prime 0 = false);;
let() = assert (is_prime 2 = true);;
let() = assert (is_prime 7 = true);;
let() = assert (is_prime 103 = true);;
let() = assert (is_prime 1000 = false);;

(* q4 *)
let rec pascal j k = 
    if k = j+1 then 0 else if k = 0 then 1 else (* if a number should be zero (out of bounds for the triangle), or if its the first column *)
      if j = 0 then 1 else (* if it's the first row *)
    pascal (j-1) k + pascal (j-1) (k-1);; (* recursively call for the top left val + directly above val *)
(* SUPER SLOW; anything over the 30th row starts to really slow down, but I guess still works - def faster way to do it *)

(* tests *)
let() = assert (pascal 5 2 = 10);;
let() = assert (pascal 7 4 = 35);;
let() = assert (pascal 1 1 = 1);;
let() = assert (pascal 15 8 = 6435);;
let() = assert (pascal 20 2 = 190);;

let pascal2 j k = (* faster alternative way using factorial recursively, but the number gets too large; experiences overflow error and returns 0 *)
  let rec factorial n = if n=0 then 1 else n * factorial (n - 1) in
  factorial j / ((factorial k) * factorial (j-k));;

(* pascal2 30 10 returns 0 *)
(* pascal 30 10 returns 30045015 after a lil bit *)





