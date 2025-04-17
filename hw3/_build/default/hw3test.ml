open Hw3
open Json
open Json_structures
(* This file provides a list of basic tests for your homework.
 * You will surely want to add more! These tests do not guarantee that your code
 * is correct or will pass our tests.
 * Notice that currently calling any of the functions on hw3.ml will fail,
 * as such, all test functions are commented by default. As you
 * work on implementing the homework functions:
 *   1. Implement the changes on hw3.ml
 *   2. Run `dune build` to make sure everything compiles
 *   3. Uncomment the corresponding test
 *   4. Add more test scenarios
 *   5. Run `dune test` to build and run your tests.
 * If working correctly, `dune test` will complete with no error messages
 *)

(* We leave the first test uncommented to get you started. Until make_silly_json
 * gets implemented, calling `dune test` or `#use "hw3test.ml"` from dune utop
 * will print a Failure message: *)
let test1 =
  assert(make_silly_json 2
  =
  Array 
    [Object [("n", Num 2.); ("b", True)];
     Object [("n", Num 1.); ("b", True)]]);;

let test2 = assert(concat_with (";", ["1"; "2"]) = "1;2")

let test3 = assert(quote_string "hello" = "\"hello\"")

let test4 = assert(string_of_json json_obj = "{\"foo\" : 3.14159, \"bar\" : [1, \"world\", null], \"ok\" : true}");;
let test4_1 = print_endline (string_of_json (Parsed_small_bus.small_bus_positions));;
print_newline
let test4_2 = print_endline (string_of_json (Parsed_medium_bus.medium_bus_positions));;

let test5 = assert(take (2, [4; 5; 6; 7]) = [4; 5])

let test6 = assert(firsts [(1,2); (3,4)] = [1; 3])
let test6_1 = assert(firsts [(1,2); (3,4); (5,6)] = [1; 3; 5])
(** don't forget to write a comment for problem 7 **)

let test8 = assert(assoc ("foo", [("bar",17);("foo",19)]) = Some 19)

let test9 = assert(dot (json_obj, "ok") = Some True)

let test10 = assert(dots (Object [("f", Object [("g", String "gotcha")])], ["f"; "g"]) = Some (String "gotcha"))
let test10_1 = assert(dots (Object [("f", True); ("h", Object [("g", String "gotcha")])], ["h"; "g"]) = Some (String "gotcha"))
let test10_2 = assert(dots (Object [("f", Object [("g", String "gotcha")])], ["h"]) = None);;
let test10_3 = assert(dots (Object [("f", Object [("g", String "gotcha")]); ("bacon", Object [("g", String "gotcha")])], ["bacon"; "g"]) = Some (String "gotcha"));;