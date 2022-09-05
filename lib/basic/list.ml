include Stdlib.List

let remove_all elem = function
  | [] -> []
  | l -> filter (fun x -> x <> elem) l

let remove_one elem = function
  | [] -> []
  | l ->
    let rec aux acc = function
      | [] -> rev acc
      | hd :: tl -> if hd = elem then rev acc @ tl else aux (hd :: acc) tl
    in
    aux [] l

(** remove elements of [l] from [l'] *)
let rec remove_list l = function
  | [] -> []
  | l' -> (
    match l with
    | [] -> l'
    | hd :: tl -> remove_list tl @@ remove_all hd l')

let random_elem = function
  | [] -> raise (Failure "random_elem")
  | l -> nth l @@ Random.int @@ length l
