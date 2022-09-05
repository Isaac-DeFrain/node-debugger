type t =
  { chain : Chain.t
  ; branch : Branch.t
  ; height : Int.t
  }
[@@deriving compare, equal]

let compare h1 h2 =
  let cmp_chain = Chain.compare h1.chain h2.chain in
  let cmp_branch = Branch.compare h1.branch h2.branch in
  (* higher heights come first *)
  let cmp_height = Int.compare h2.height h1.height in
  if cmp_chain < 0 || cmp_chain > 0 then cmp_chain
  else if cmp_branch < 0 || cmp_branch > 0 then cmp_branch
  else cmp_height

let view h =
  Printf.sprintf "Header(chain %s, branch %s, height %d)"
    Chain.(view h.chain)
    Branch.(view h.branch)
    h.height
