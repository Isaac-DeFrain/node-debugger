type t = Block.t list

let empty : t = []

let insert b = function
  | [] ->
      [b]
  | bs ->
      b :: bs |> List.sort_uniq Block.compare

let rec remove b = function
  | [] ->
      []
  | hd :: tl as bs ->
      let cmp = compare b hd in
      if cmp = 0 then tl else if cmp < 0 then bs else hd :: remove b tl

let to_list bs = bs

let chain_list blocks =
  let open Block in
  List.map (fun b -> b.header.chain) blocks |> List.sort_uniq Chain.compare

let blocks_on_chain c =
  let open Block in
  List.filter (fun b -> b.header.chain = c)

let view_chain bs c =
  String.concat ", " (List.map Block.view (blocks_on_chain c bs))

let view = function
  | [] ->
      "[]"
  | bs ->
      let blk_str =
        List.map (view_chain bs) (chain_list bs) |> String.concat ",\n  "
      in
      Printf.sprintf "[%s]" blk_str
