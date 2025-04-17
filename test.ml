let valid = [31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31];;
(*let test = List.nth valid 2;;
 print_int(test);;*)

let isValid (month:int) (day:int) =
    if day <= List.nth valid (month-1) then true else false;;
let test = assert (isValid 1 1 = true);;