(* example code for mutual recursions and local functions, 
   with some code from book section 2.4.
   The problem here is to determine whether n is even. *)

(* non-recursive solution*)
(* [even_simple n] returns whether [n] is even. *)
let even_simple n = (n mod 2 = 0)

(* a solution using mutually recursive functions*)
(* [even1 n] returns whether [n] is even.
Requires: [n >= 0]. *)
let rec even1 n =
  n = 0 || odd (n - 1)

(** [odd n] returns whether [n] is odd.
Requires: [n >= 0]. *)
and odd n =
  n <> 0 && even1 (n - 1);;

(* a solution using a local helper function for mutual recursion*)
(* [even2 n] returns whether [n] is even.
Requires: [n >= 0]. *)
let rec even2 n =
   let 
      (** [odd_local n] is whether [n] is odd.
      Requires: [n >= 0]. *)
      odd_local n = n <> 0 && even2 (n - 1)
   in
      n = 0 || odd_local (n - 1)

(* bind even to one of the solutions, and test it with test cases *)
let even = even2;;  

(* testing *)
assert (even 0 = true);;
assert (even 2 = true);;
assert (even 100 = true);;
assert (even 1 = false);;
assert (even 3 = false);;

assert (odd 1 = true);;
assert (odd 3 = true);;
