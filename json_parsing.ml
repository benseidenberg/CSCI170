include Json

type token =
| NumLit of string (* eg, number 3.14 represented as "3.14" *)
| StringLit of string (* eg, "foo" *)
| FalseTok (* false *)
| TrueTok  (* true *)
| NullTok  (* null *)
| LBrace   (* { *)
| RBrace   (* } *)
| LBracket (* [ *)
| RBracket (* ] *)
| Comma    (* , *)
| Colon    (* : *)

(* For debugging purposes, it will be convenient to be able to print
   tokens. Here is a provided function to convert tokens to strings. *)
let string_of_token t = 
match t with
  NumLit    s ->  s
| StringLit s -> quote_string s
| FalseTok    -> "false"
| TrueTok     -> "true"
| NullTok     -> "null"
| LBrace      -> "{"
| RBrace      -> "}"
| LBracket    -> "["
| RBracket    -> "]"
| Comma       -> ","
| Colon       -> ":"

(* helper function that converts a character to a string *)
let char_to_string = String.make 1

(* helper function that converts a list of characters to a string *)
let string_of_char_list xs = String.concat "" (List.map char_to_string xs)

(* helper function that converts a string to a list of characters *)
let char_list_of_string s = List.of_seq (String.to_seq s)

(* helper function that checks whether a character is on alphabet *)
let is_alpha c =
  match c with
    'a' .. 'z' -> true
  | 'A' .. 'Z' -> true
  | _ -> false

(* helper function that checks whether a character is a digit *)
let is_digit c =
  match c with
    '0' .. '9' -> true
  | _ -> false

exception LexicalError of string
(* Helper function to report lexical errors conveniently.
   ("Lexical analysis" is another name for "tokenization".) *)
let lexical_error msg = raise (LexicalError ("Lexical error: " ^ msg))



(* PProblem 1: Write a consumer for string literals. String literals
   are required to be quoted in JSON, so your consumer should look for
   a double quote, and then keep consuming characters until it sees
   the closing double quote.

   Call `lexical_error` with an appropriate message if there is
   no string literal at the beginning of the given character list.

   Also call `lexical_error` if the string literal at the beginning of
   the list has no closing double quote.

   Hint: After accepting the opening double quote, use a helper
         function to recursively look for the closing double quote,
         returning the string of characters in between, and the remainder.
*)
let consume_string_literal cs = 
  (* TODO: add about 10 lines of code to this function, and edit the "failwith" line below. *)
  match cs with
    '\"' :: cs -> failwith "consume_string_literal should now call a helper function to consume rest of string"
  | _ -> lexical_error "Expecting string literal."


(* Problem 2: Write a consumer for the keywords true, false, and null.

   Call `lexical_error` with an appropriate message if the given
   character list does not start with a keyword.

   Hint: One way to do this is to use pattern matching to look for
         the sequence of characters for each keyword.

   Hint: Another (in our opinion, cleaner) way is to write a helper
         consumer that looks for an *arbitrary* sequence of alphabetic
         characters, and then to use `assoc` from the homework assignment
         to convert the string to a token using a lookup table such as
             [("true", TrueTok), ("false", FalseTok), ("null", NullTok)].

         (This takes advantage of the fact that assoc has a relatively
         general type, allowing the second components of the pairs to have
         *any* type whatsoever.)

         Remember that assoc returns an option -- report a lexical error as 
         appropriate.

         You can check whether a character `c` is alphabetic using the
         provided helper function is_alpha.

   Either of the above strategies will receive full credit.
*)
let consume_keyword cs = 
  (* TODO, about 15 lines *) failwith "consume_keyword unimplemented"

(* Here's a provided consumer for numbers, since it's a bit complex.
   You shouldn't need to understand this code unless you want to.

   JSON uses a fairly common textual format for floating point numbers.
   The format described in Section 6 of RFC 7159, which we summarize here.

   A number consists of an optional minus sign, followed by an integer
   part, followed by an optional fractional part, followed by an
   optional exponent part.

       number = [ '-' ] int [ frac ] [ exp ]

   where single quotes enclose literal characters and square brackets
   denote optional components.

   The integer part is a nonempty sequence of digits, which may only
   start with a 0 if the entire integer part is 0.

       int = '0'
           | nonzero-digit digit*

       nonzero-digit = '1' | '2' | ... | '9'

       digit = '0' | nonzero-digit

   where vertical bars (|) denote alternatives and stars ( * ) denote
   0-or-more repetitions.

   The fractional part is a period followed by one or more digits.

       frac = '.' digit+

   where plus (+) denotes 1-or-more repetitions.

   The exponent part is a letter E (lowercase or uppercase), followed
   by an optional sign (minus or plus), followed by one or more
   digits.

       exp = e [ sign ] digit+

       e = 'e' | 'E'

       sign = '-' | '+'

   We structure consume_num in terms of several "helper consumers"
   that consume the various parts described above, such as the
   optional leading minus sign, or the fractional part, and so on.
*)
let consume_num cs = 
  let consume_minus xs = 
      match xs with
        ('-' :: cs) -> (['-'], cs)
      | cs -> ([], cs)
  in
  let consume_exp_sign xs = 
      match xs with
        ('-' :: cs) -> (['-'], cs)
      | ('+' :: cs) -> (['+'], cs)
      | cs -> ([], cs)
  in
  let consume_digit_list cs =
    let rec loop pr = 
      match pr with
        (acc, []) -> (List.rev acc, [])
      | (acc, (c :: cs)) ->
        if is_digit c then 
          loop ((c :: acc), cs)
        else 
          (List.rev acc, c :: cs)
    in
    loop ([], cs)
  in
  let consume_frac xs = 
    match xs with
      ('.' :: cs) ->
      let (l, cs) = consume_digit_list cs in
      ('.' :: l, cs)
    | cs -> ([], cs)
  in
  let consume_exp xs = 
    match xs with
    (c :: cs) ->
    if c = 'e' || c = 'E' then
      let (sign, cs) = consume_exp_sign cs in
      let (l, cs) = consume_digit_list cs  in
      (c :: sign @ l, cs)
    else 
      ([], c :: cs)
  | [] -> ([], [])
  in
  let (minus, cs) = consume_minus      cs in
  let (int,   cs) = consume_digit_list cs in
  let (frac,  cs) = consume_frac       cs in
  let (exp,   cs) = consume_exp        cs in
  (string_of_char_list (minus @ int @ frac @ exp), cs)


(* We now have all the consumers we need to write the main tokenizer loop. *)

(* Problem 3: Complete the following definition of `tokenize_char_list`
   that implements the tokenizer.

   Call `lexical_error` with an appropriate message if you encounter an
   unexpected character. (Use char_to_string to get a printable
   representation of a character.)

   Hint: You'll need to have one branch per kind of token, plus a few
         more to skip whitespace.

   Hint: Use the consumers from above.

   Hint: Remember to look for whitespace so that you can correctly ignore it.
 *)
let tokenize_char_list cs = 
  let rec go (cs,acc) =
    match cs with
      [] -> List.rev acc
    | '\n' :: cs -> go (cs, acc)  (* ignore newlines *)
    | '{'  :: cs -> go (cs, (LBrace :: acc))
    (* TODO, about 7 lines: several more cases here *)
    | c :: cs ->
       if is_digit c || c = '-'
       then
         let (s, cs) = consume_num (c :: cs) in
         go (cs, (NumLit s :: acc))
       else (* TODO, about 15 lines: check for string literals and keywords here 
               and call the corresponding consumer. otherwise, call lexical error
               as below. *)
         lexical_error ("Unknown character " ^ char_to_string c)
  in
  go (cs, [])

(* Problem 4: Write the top level lexer that takes a string,
   converts it to a char list, and passes it to `tokenize_char_list`.

   Hint: use char_list_of_string and tokenize_char_list *)
let tokenize (s : string) : token list =
    (* TODO, 1 line *) failwith "tokenize unimplemented"

(** Trye to finish the functions above by the first milestone, Nov. 17*)

(* First, here's a provided function to report a syntax error. It takes
   the current token list and a message, and raises an exception. It
   uses the token list to print out the current token (or "EOF"
   standing for "end of file" if there are no tokens left), which
   helps when debugging to know where in the token list the error
   occurred. *)
   exception SyntaxError of string
   let syntax_error (ts, msg) = 
     let tokenName =
       match ts with
         [] -> "EOF"
       | t :: _ -> string_of_token t
     in
     raise (SyntaxError ("Syntax error at " ^ tokenName ^ ": " ^ msg))
   
   (* As a very simple (but still useful below) example, we can write a
      parser that consumes a string literal at the beginning of the token
      list. *)
   
   (* Problem 5: write a `parse_string` function that consumes a string
      literal at the beginning of the token list and returns it.
   
      If there is no string literal at the beginning of the token list,
      call `syntax_error` with the token list and an appropriate message.
   *)
   let parse_string (ts : token list) : string * token list =
     (* TODO, about 3 lines *) failwith "parse_string unimplemented"
   
   (* It is often useful to consume a single token from the token list
      and throw it away, returning the rest of the tokens and throwing an
      error if the token was not there. *)
   
   (* Problem 6: write a function `expect` which consumes a single,
      specific token from the token list.
   
      If the token is not there as expected, call syntax_error with an
      appropriate message. *)
   let expect (t, ts) = 
     (* TODO, about 6 lines *) failwith "expect unimplemented"
   
   
   (* We're now ready to start writing a `parse_json` function, which
      will contain several local helper functions. In this case, it is
      important that these helper functions be local and not at the top
      level, because they need to recursively call `parse_json`.
   
      This also makes these functions much more difficult to test, since
      they are not available at the top level scope. (There are ways
      around this, eg, using mutual recursion instead of nested
      functions.) So you'll just need to test `parse_json` extra
      thoroughly to ensure each local helper function is working properly.
   
      You may want to skip the helper function problems at first and
      work on the main body of parse_json (after the let...in), where
      you can parse everything except objects and arrays, and then come
      back to the helper functions later to complete the function.
   *)
   let rec parse_json (ts : token list) : json * token list =
       (* Problem 7: write a `parse_field_value` function that parses one
          field-value pair in an object.
   
          The syntax for a field-value pair is a string literal,
          representing the field name, followed by a colon, followed by
          an arbitrary json value.
   
          Hint: use `parse_string` for the field name, `expect` for the
                colon, and a recursive call to `parse_json` for the value. *)
     let parse_field_value (ts : token list) : (string * json) * token list =
       (* TODO, about 7 lines *) failwith "parse_field_value unimplemented"
     in
       (* Problem 8: write a function `parse_field_value_list` that
          parses a possibly empty comma-separated list of field-value
          pairs, terminated by a closing brace. (This will be used below
          to parse strings representing objects, which are always
          surrounded in braces.)
   
          Hint: use parse_field_value to parse each field-value pair.
   
          Hint: First check to see if the first token is a closing
                brace. If so, immediately return the empty list.
                Otherwise, parse a field-value pair and then check
                whether the next token is a comma. If so, consume it an
                recursively parse a list of field-value pairs, and then
                cons the new field-value pair on the front. If it is not a comma,
                immediately return a singleton list.
        *)
     let rec parse_field_value_list (ts : token list) : (string * json) list * token list =
       (* TODO, about 15 lines *) failwith "parse_field_value_list unimplemented"
     in
       (* Problem 9: Write a function `parse_array_element_list` that
          parses a possibly empty comma-separated list of json values,
          terminated by a closing square bracket.
   
          Hint: this is very similar to `parse_field_value_list`, except
                that it calls `parse_json` instead of
                `parse_field_value`, and uses square brackets instead of
                curly braces.
        *)
     let rec parse_array_element_list (ts : token list) : json list * token list =
       (* TODO, about 15 lines *) failwith "parse_array_element_list unimplemented"
     in
     (* Problem 10: complete the definition of `parse_json` by adding
          branches to the pattern match below.
   
          If the beginning of the token list does not represent a json value,
          call `syntax_error` with an appropriate message.
   
          Hint: Very little new code needs to be written in each branch.
                Call the helper functions above as appropriate.
        *)
     match ts with
       NumLit s :: ts -> begin
       match Float.of_string_opt s with
         Some r -> (Num r, ts)
       | None -> syntax_error (ts, "bad float")
       end
     (* TODO, about 10 lines: more cases here *)
     | _ -> syntax_error (ts, "expecting json")
   
   (* Here is a provided function to parse a .json file, using your parser above. *)
   let parse_from_file file_name = 
     let ic = open_in file_name in
     let read_to_end () =
       let rec go buf =
         match input_line ic with
         | line ->
            Buffer.add_string buf line;
            Buffer.add_char buf '\n';
            go buf
         | exception End_of_file ->
           close_in ic;
           Buffer.contents buf
       in
       go (Buffer.create 256)
     in
     let input = read_to_end () in
     let ts = tokenize input in
     let (j, _) = parse_json ts in
     j