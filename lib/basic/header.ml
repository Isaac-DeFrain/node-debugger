type t = { chain : Chain.t; branch : Branch.t; height: int }

let compare h1 h2 =
  let cmp_chain = Chain.compare h1.chain h2.chain in
  let cmp_branch = Branch.compare h1.branch h2.branch in
  (* higher heights come first *)
  let cmp_height = compare h2.height h1.height in
  if cmp_chain < 0 || cmp_chain > 0 then cmp_chain
  else if cmp_branch < 0 || cmp_branch > 0 then cmp_branch
  else cmp_height

let view h = "Header(" ^
  String.concat ", "
    [ "chain: " ^ Chain.view h.chain
    ; "branch: " ^ Branch.view h.branch
    ; "height: " ^ string_of_int h.height
    ] ^ ")"
