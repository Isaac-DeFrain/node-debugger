open! List
open! Base

type t = Message.t list [@@deriving compare, equal]

let empty = []

let length = length

let add m ms = sort ~compare:Message.compare (m :: ms)

let remove = remove_one

let to_list msgs = msgs

let of_list msgs = sort ~compare:Message.compare msgs

let view msgs =
  Caml.Printf.sprintf "[ %s ]"
  @@ String.concat ~sep:", " (map ~f:Message.view msgs)
