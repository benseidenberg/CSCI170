(* fib *)
let rec fib i = 
  if i = 1 || i = 2 then 1 else fib (i-1) + fib (i-2);;

let() = assert(fib 4 = 3);;
let() = assert(fib 8 = 21);;
let() = assert(fib 10 = 55);;

(* RMS; sqrt((x^2+y^2)/2) *)
let rms x y = sqrt(float_of_int((x*x + y*y)/2));;

(* average infix operator *)
let (+/.) x y = (x+.y)/.2.;;

print_float(1.0 +/. 2.0);;
print_endline "Hello World!";;
print_string "Hello world!";;

(* fast fib *)
let fast_fib n = 
  let rec h n pp p = 
    if n = 1 then p 
    else (h (n-1) p (pp+p)) 
  in h n 0 1;;

  
