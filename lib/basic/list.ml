include Stdlib.List

let remove elem = function
  | [] -> []
  | l -> filter (fun x -> x <> elem) l
