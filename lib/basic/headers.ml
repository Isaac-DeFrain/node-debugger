type t = Header.t list

let empty = []

let insert h = function
| [] -> [h]
| hs -> h::hs |> List.sort_uniq Header.compare

let rec remove h = function
| [] -> []
| hd::tl as hs ->
  let cmp = compare h hd in  
  if cmp = 0 then tl
  else if cmp < 0 then hs
  else hd :: remove h tl

let to_list hs = hs

let view = function
| [] -> "[]"
| hs -> "[" ^ String.concat ", " (List.map Header.view hs) ^ "]"
