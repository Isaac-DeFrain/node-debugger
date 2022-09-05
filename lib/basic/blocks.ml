open! Base

type t = Block.t list [@@deriving compare, equal]

let empty : t = []

let sort compare = List.dedup_and_sort ~compare

let insert b = function
  | [] -> [ b ]
  | bs -> b :: bs |> sort Block.compare

let rec remove b = function
  | [] -> []
  | hd :: tl as bs ->
    let cmp = Block.compare b hd in
    if cmp = 0 then tl else if cmp < 0 then bs else hd :: remove b tl

let of_list = sort Block.compare

let to_list bs = bs

let chain_list blocks =
  let open Block in
  List.map ~f:(fun b -> b.header.chain) blocks |> sort Chain.compare

let blocks_on_chain c =
  let open Block in
  List.filter ~f:Chain.(fun b -> equal b.header.chain c)

let view_chain bs c =
  String.concat ~sep:", " (List.map ~f:Block.view (blocks_on_chain c bs))

let view = function
  | [] -> "[]"
  | bs ->
    let blk_str =
      List.map ~f:(view_chain bs) (chain_list bs) |> String.concat ~sep:",\n  "
    in
    Printf.sprintf "[%s]" blk_str
