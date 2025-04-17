(* q1 *)
let rec odds lst = 
  match lst with 
  | [] -> [] (* if empty return empty *)
  | h :: t -> if (h mod 2 ==1) then h :: odds t else odds t;; (* if the value h mod 2 == 1 it's odd, so h :: odds t adds h to front of new list and calls odds t, else ignore h*)

let() = assert(odds [1] = [1]);;
let() = assert(odds [1;3] = [1;3]);;
let() = assert(odds [2] = []);;
let() = assert(odds [1;2;3;4;5] = [1;3;5]);;

(* q2 *)
let first_index num lst = 
  let rec find_s index num hd = (*helper function to keep track of index*)
  match hd with 
  | [] -> -1 (*not in list*)
  | h :: t -> if h == num then index else find_s (index+1) num t (*if h is num, return the index, else advance list and increase index by one*)
  in find_s 0 num lst (*start call with index = 0*)

let() = assert(first_index 20 [20;15;20] = 0)
let() = assert(first_index 15 [20;15;20] = 1)
let() = assert(first_index 0 [20;15;20] = -1)
let() = assert(first_index 5 [1;2;3;4;5] = 4)

(* q3 *)
let partial_sum lst = 
  let rec adder sum lst = (* helper func to keep track of sum *)
    match lst with
    | [] -> [] (* no sum *)
    | h :: t -> h+sum :: adder (h+sum) t (* h + sum, then call function again with new sum + rest of list*)
  in adder 0 lst;; (* call starting with sum of 0 *)

let() = assert(partial_sum [2; (-7); 100] = [2; -5; 95])
let() = assert(partial_sum [1; 2; 3; 4] = [1;3;6;10])
let() = assert(partial_sum [0] = [0])
let() = assert(partial_sum [(-5); 100; 69; 32; 123] = [-5; 95; 164; 196; 319])

(* q4 *)
let pascal_tr j k =  (* SO SLOW BUT IT WORKS *)
  let rec h j k row pp p = 
    if  row = j then pp+p (* when hits row, add last two nums together *)
    else if k = 0 || k = j then 1 (* base cases *)
    else (h j k (row+1) (h (j-1) (k-1) (row-1) pp p) (h (j-1) k (row-1) pp p)) (* increases row count while calculating the previous row's pp & p*)
  in h j k 1 0 1;;  


let pascal_tr_fast j k = (* based on the nCr calculation of pascal's triangle *)
  let rec calc n prev =
    if n > k then prev
    else calc (n + 1) (prev * (j - n + 1) / n) (* uses j!/(k!(j-k)!) formula of nCr (direct relation with pascal's triangle);
      using the recurrent formula that nCr = nsC(r-1) * n-r+1/r*)
  in
  calc 1 1;;

let() = assert (pascal_tr 5 2 = 10);;
let() = assert (pascal_tr 7 4 = 35);;
let() = assert (pascal_tr 1 1 = 1);;
let() = assert (pascal_tr_fast 15 8 = 6435);; (* original pascal_tr way too slow *)
let() = assert (pascal_tr_fast 20 2 = 190);;
  