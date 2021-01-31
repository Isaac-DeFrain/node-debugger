type t = Message.t list

open List

let empty = []

let length = length

let add m ms = sort Message.compare (m :: ms)

let remove = remove_one

let to_list msgs = msgs

let of_list msgs = msgs

let view msgs = "[" ^ String.concat ", " (map Message.view msgs) ^ "]"
