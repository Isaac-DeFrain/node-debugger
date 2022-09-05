type t =
  { header : Header.t
  ; ops : Ops.t
  }

let block header ops = { header; ops }

let block' c b height num_ops =
  let open Header in
  let hdr = { chain = Chain.id c; branch = Branch.id b; height } in
  let ops = Ops.ops height num_ops in
  block hdr ops

let compare b1 b2 =
  let h_cmp = Header.compare b1.header b2.header in
  if h_cmp < 0 || h_cmp > 0 then h_cmp else Ops.compare b1.ops b2.ops

let view b =
  Printf.sprintf "Block(%s, ops: %s)" Header.(view b.header) Ops.(view b.ops)
