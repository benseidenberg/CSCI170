
(* type definition that is used throughout the assignment *)
type json =
  | Num of float
  | String of string
  | False
  | True
  | Null
  | Array of json list
  | Object of (string * json) list

(* some json values, useful for testing *)
let json_pi    = Num 3.14159
let json_hello = String "hello"
let json_false = False
let json_array = Array [Num 1.0; String "world"; Null]
let json_obj   = Object [("foo", json_pi); ("bar", json_array); ("ok", True)]
