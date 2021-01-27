include Stdlib.Queue
  
let to_list q =
  let cpy = copy q in
  let rec aux acc q =
  if is_empty q then acc
  else let p = pop q in aux (p :: acc) q
  in
  aux [] cpy

let of_list = function
| [] -> create ()
| l ->
  let rec aux acc = function
  | [] -> acc
  | hd::tl -> push hd acc; aux acc tl
  in
  aux (create ()) l
