open OUnit2
open Json
open Json_parsing

(* ---- Tokenizing Tests: add more of your own ---- *)

let keyword_true_test _ =
  let input = ['t'; 'r'; 'u'; 'e'] in
  let expected = (TrueTok, []) in
  assert_equal expected (consume_keyword input)


(* ---- Tokenizing Test Suite:  ---- *)
let tokenize_test =
  "json_parsing tests" >::: [
    "true keyword test" >:: keyword_true_test;
  ]

(* ---- Parsing Tests: add more of your own ---- *)

  (* Helper: Parse a JSON string and return the JSON value *)
  (* This function assumes the input string is well-formed JSON *)
  (* and does not handle errors. *)
  (* [fst] is OCaml function to get the first element of a 2-tuple*)
  let get_json s = s |> tokenize |> parse_json |> fst

  (* ---- Basic Parsing Tests ---- *)
  let test_parse_null _ =
    assert_equal Null (get_json "null")
 

  let test_parse_true _ =
    assert_equal (True) (get_json "true")
  
  let test_parse_false _ =
    assert_equal (False) (get_json "false")
  
  let test_parse_number _ =
    assert_equal (Num 42.5) (get_json "42.5")
  
  let test_parse_string _ =
    assert_equal (String "hello") (get_json "\"hello\"")
  
  (* ---- Composite Types ---- *)

  let test_parse_array_of_numbers _ =
    assert_equal 
      (Array [Num 1.5; Num 2.7; Num 3.4])
      (get_json "[1.5,  2.7,  3.4]")
    
  let test_parse_simple_object _ =
    assert_equal
      (Object [("name", String "Alice"); ("age", Num 30.9)])
      (get_json "{\"name\": \"Alice\", \"age\": 30.9}")
  
  (* ---- Error Handling ----*)

   let test_unclosed_array _ =
    try
      let _ = get_json "[1, 2" in
      assert_failure "Expected an exception, but none was raised"
    with
    | _ -> ()  (* Pass: any exception was raised *)
 
    let test_unclosed_object _ =
      try
        let _ = get_json "{\"a\": 1" in
        assert_failure "Expected an exception, but none was raised"
      with
      | _ -> ()  (* Pass: any exception was raised *)
  
  (* ---- Nested Structures ---- *)
  let test_parse_nested_object _ =
    assert_equal
      (Object [("user", Object [("name", String "Bob")])])
      (get_json "{\"user\": {\"name\": \"Bob\"}}")
  
  let test_parse_array_in_object _ =
    assert_equal
      (Object [("ids", Array [Num 1.5; Num 2.3])])
      (get_json "{\"ids\": [1.5, 2.3]}")
    
  
  (* ---- Parsing Test Suite ---- *)
  let parsing_test =  
    "get_json tests" >::: [
      "null" >:: test_parse_null;
      "bool true" >:: test_parse_true;
      "bool false" >:: test_parse_false;
      "number" >:: test_parse_number;
      "string" >:: test_parse_string;
      "array of numbers" >:: test_parse_array_of_numbers;
      "simple object" >:: test_parse_simple_object;
       "unclosed array" >:: test_unclosed_array;
      "unclosed object" >:: test_unclosed_object;
      "nested object" >:: test_parse_nested_object;
      "array in object" >:: test_parse_array_in_object;
  ] 

let suite =
  "Json Parsing Tests" >::: [
    tokenize_test;
    parsing_test;
  ]

(* Run the test suite *)
let () = run_test_tt_main suite