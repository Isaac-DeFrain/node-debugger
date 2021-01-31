type t = int * int

let ops h n = (h, n)

let compare (h1, n1) (h2, n2) =
  let cmp_height = compare h2 h1 in
  if cmp_height < 0 || cmp_height > 0 then cmp_height else compare n1 n2

let view (h, n) = Printf.sprintf "(%d, %d)" h n
