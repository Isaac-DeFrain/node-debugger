include Base.List

let compare_list l1 l2 =
  let lt (x, y) = Core.Poly.compare x y < 0 in
  let leq (x, y) = Core.Poly.compare x y <= 0 in
  if for_all ~f:lt @@ zip_exn l1 l2 then -1
  else if for_all ~f:leq @@ zip_exn l1 l2 then 0
  else 1

let remove_all elem = function
  | [] -> []
  | l -> filter ~f:(fun x -> x <> elem) l

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
  | l -> nth_exn l @@ Random.int @@ length l

let equal_list = equal
