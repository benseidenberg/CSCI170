type student = {first_name : string; last_name : string; gpa : float};;

let ben = {first_name="ben"; last_name="seidenberg"; gpa=4.1};;

let name1 student = student.first_name, student.last_name;;

let sr_builder f l g = {first_name=f; last_name=l; gpa=g};;


let safe_hd lst =
  match lst with 
  | [] -> None
  | h::t -> Some h

type poketype = Normal | Fire | Water
type pokemon = {name : string; hp : int; ptype : poketype}
let max_hp lst = 
  match lst with 
  | [] -> None
  | _ ->
  let rec finder lst max = 
    match lst with
    | [] -> Some max
    | h::t -> if (h.hp > max.hp) then finder t h else finder t max
  in finder lst {name="Null"; hp=0; ptype=Normal};;

let charizard = {name="charizard"; hp=73; ptype=Fire}
let squirtle = {name="squirtle"; hp=33; ptype=Water}
let bacon = {name="bacon"; hp=53; ptype=Normal}