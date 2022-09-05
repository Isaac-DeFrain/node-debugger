open! Base

type t = Header.t list [@@deriving compare, equal]

let empty = []

let insert h = function
  | [] -> [ h ]
  | hs -> h :: hs |> List.dedup_and_sort ~compare:Header.compare

let rec remove h = function
  | [] -> []
  | hd :: tl as hs ->
    let cmp = Header.compare h hd in
    if cmp = 0 then tl else if cmp < 0 then hs else hd :: remove h tl

let of_list = List.dedup_and_sort ~compare:Header.compare

let to_list hs = hs

let view = function
  | [] -> "[]"
  | hs ->
    Caml.Printf.sprintf "[ %s ]"
    @@ String.concat ~sep:", " (List.map ~f:Header.view hs)
