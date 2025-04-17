(* This is from file json.ml in this directory. json.ml
 * contains the main datatype definition we will use throughout the
 * assignment. You will want to look over this file before starting. *)
 include Json

 (* These come from the parsed_*_bus.ml.
    Each file binds one variable: small_bus_positions (10 reports),
    medium_bus_positions (100 reports), and complete_bus_positions (~1000 reports),
    respectively with the data that you will need to implement 
    your homework.
 *)
 open Json_structures.Parsed_complete_bus
 open Json_structures.Parsed_medium_bus
 open Json_structures.Parsed_small_bus
 
 (* provided helper function that deduplicates a list *)
 let dedup xs = List.sort_uniq compare xs;;
 
 (* provided helper function that sorts a given list *)
 let sort xs = List.sort compare xs;;
 
 (* provided helper function to convert a float to a string *)
 (* OCaml's string_of_float is not quite RFC compliant due to its tendency
    to output whole numbers with trailing decimal points without a zero.
    But, printf does the job how we want. *)
 let json_string_of_float f =
   Printf.sprintf "%g" f;;
   
 (* 1 *)
 let make_silly_json i =
   let rec list_builder obj i = (* helper func *)
     if i = 1 then (Array (obj @ [Object [("n", Num (float_of_int i)); ("b", True)]])) else (* if its the final value, just return Array json list of all objs *)
     list_builder (obj @ [Object [("n", Num (float_of_int i)); ("b", True)]]) (i-1) (* append i-1 json object to rest of objs recursively *)
   in list_builder [] i;; (* starter call *)
   (*failwith "Need to implement: make_silly_json"*)
 
 (* 2 *)
 let rec concat_with (sep, ss) =
   match ss with 
   | [] -> "" (* if empty return empty string *)
   | h::t -> if (t <> []) then h ^ sep ^ concat_with (sep, t) else h;; (* used if/else since it was adding an extra sep at the end of the string, so account for that if tail is empty*)
   (*failwith "Need to implement: concat_with"*)
 
 (* 3 *)
 let quote_string s =
   {|"|} ^ s ^ {|"|} (* just adds quotation marks using weird Ocaml formatting *)
   (*failwith "Need to implement: quote_string"*)
 
 (* 4 *)
 let rec string_of_json j =
  let rec obj_str obj = (* Helper function to convert an object to a string *)
    match obj with
    | Object [] -> ""  (* Empty object case *)
    | Object [(n, d)] -> (* Single key-value pair: "key" : value *)
        concat_with (" : ", [string_of_json (String n); string_of_json d])
    | Object ((n, d) :: t) -> (* Multiple key-value pairs *)
        concat_with (", ", 
          [concat_with (" : ", [string_of_json (String n); string_of_json d]); obj_str (Object t)]
        )
    | _ -> ""  (* extra just in case *)
  in
  let rec arr_str arr =   (* Helper function to convert an array to a string *)
    match arr with
    | [] -> ""  (* Empty array case *)
    | [item] -> string_of_json item  (* Single-element array *)
    | h :: t -> concat_with (", ", [string_of_json h; arr_str t])  (* Multiple elements *)
  in
  match j with   (* Main func *)
  | Num n -> json_string_of_float n  (* Convert number to string *)
  | String s -> quote_string s  (* Wrap string in quotes *)
  | True -> "true"
  | False -> "false"
  | Null -> "null"
  | Array a -> "[" ^ arr_str a ^ "]"  (* Format arrays *)
  | Object o -> "{" ^ obj_str (Object o) ^ "}"  (* Format objects *)
 
 (* 5 *)
 let rec take (n,xs) = 
   if n = 0 then [] else
   match xs with
     | [] -> []
     | h::t -> [h] @ take ((n-1), t);;
   (*failwith "Need to implement: take"*)
 
 (* 6 *)
 let rec firsts xs = 
   match xs with 
   | [] -> []
   | [(first, last)] -> [first]
   | h::t -> match h with 
     | (first, last) -> [first] @ firsts t;;
 
 (* 7 *)
 (* They should always evalutate to the same result. firsts (take(n, xs)) will see each pair in xs and return the first n elements, and then firsts will then take the first of those n
 Conversely, take (n, firsts xs) will see (firsts xs) make a list of all the first elements, and then trim it down to the first n of those. 
 i believe the first variation will be faster since it requires less iteration through the lists, going through only n elements each time O(n*n), instead of the length of xs and then n O(ln(xs)*n) *)
 
 (* 8 *)
 let rec assoc (k, xs) =
   match xs with
   | [] -> None
   | h::t -> match h with 
     | (k1, v1) -> (match k1=k with
        | true -> Some v1
        | false -> assoc (k,t));;
 (* 9 *)
 let dot (j, f) = 
   match j with 
   | Object o -> assoc (f, o)
   | _ -> None
 
 (* 10 *)
 let rec dots (j, fs) =
  match fs with
  | [] -> Some j  (* If there are no more fields to follow, return the current JSON object *)
  | h::t -> 
      match j with
      | Object o -> (match assoc (h, o) with
          | Some next_j -> dots (next_j, t) (* Continue with the next field *)
          | None -> None)
      | _ -> None  (* If `j` is not an object but we still have fields left, return None *)
