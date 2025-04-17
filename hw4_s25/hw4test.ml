open OUnit2
open Hw4
open Json

(* This file provides some example tests, and you need to add more. *)

 
(* [make_test] is a helper function that creates a test case for funciton 
    [f] with the input [argument] and the [expected] ouput. [test_info] gives
    test information, useful for identifying the test case in the error 
    message if the test is not passed. *)
let make_test test_info f argument expected =
  test_info >:: (fun _ -> assert_equal expected (f argument))


(* 1 *)
let tests_one_fields = "test suite for one_fields" >::: [
  make_test "example json_object" one_fields json_obj ["ok"; "bar";"foo"];
] (* add more tests*)


(* 2 *)
let tests_no_repeats = "test suite for no_repeats" >::: [
  make_test "false case" no_repeats ["foo";"bar";"foo"] false;
] (* add more tests*)

(* 3 *)
let nest1 = Array [Object [];
                  Object[("a",True);
                         ("b",Object[("foo",True);
                                     ("foo",True)]);
                         ("c",True)];
                  Object []];;

let tests_recursive_no_field_repeats = "test suite for recursive_no_field_repeats" >::: [
  make_test "false case, nest1" recursive_no_field_repeats nest1 false;
] (* add more tests*)


(* 4, count count_occurrences*)
(* The test suite below includes the tests for an empty list and an 
unsorted list, with the latter returning an exception. Check how
the exception is tested, and refer to textbook section 3.10 
for more information.*)
let tests_count_occurrences = "test suite for count_occurrences" >::: [
  make_test "empty list" count_occurrences ([], (Failure "")) [];
  "not sortd exception" >:: ((fun _ -> assert_raises (Failure "not sorted") 
  (fun () -> count_occurrences (["b"; "a"], (Failure "not sorted")))));
]
(* Another test for count_occurences: 
Argument: (["a"; "a"; "b"], (Failure ""))
Expected value: [("b", 1); ("a", 2)] or [("a", 2); ("b", 1)]

Our problem specification allows any order in the resulting 
list, as long as it contains the correct items. Choose the order 
for the expected value in your own test aligns with your code’s 
logic, and be sure to understand why your code produces the 
result list in that particular order.

Also, add more tests *)


(* lists for the testing of later functions -- you need to add more *)
let lst_random = ["Y"; "Joy"; "aB"; "Eg"; "all"; "dog"]

(* 5, lowercase_start_only *)
(* below is one testcase. You need to define your own test suite
for this function, add this case and additional ones to it.
argument: lst_random
expected value:  ["aB"; "all"; "dog"] *)


(* 6, longest_string *)
(* same instructions as above.
one test case:
argument: lst_random
expected value: "Joy"
*)

(* 7, longest_lowercase_start *)
(* same instructions as above.
one test case:
argument: lst_random
expected value: "all"
*)

(* 8, caps_no_X_string *)
(* same instructions as above.
one test case:
argument: "aBxXXxDdx"
expected value: "ABDD"
*)


(* Overarching test suite combining all individual suites *)
  let all_tests = "all tests" >::: [
    tests_one_fields;
    tests_no_repeats; 
    tests_recursive_no_field_repeats;
    tests_count_occurrences;
    (* add test suites for other functions *)
  ]
  
  (* Run all tests *)
  let () =
    run_test_tt_main all_tests
  