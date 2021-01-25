open Basic

(* parameters: chain *)
module CMap = Map.Make(Chain)

(* parameters: chain, branch *)
module CBMap = Map.Make(struct
  type t = Chain.t * Branch.t
  let compare (c1, b1) (c2, b2) =
    let c_cmp = compare c1 c2 in
    if c_cmp < 0 || c_cmp > 0 then c_cmp
    else compare b1 b2
end)

(* parameters: chain, node *)
module CNMap = Map.Make(struct
  type t = Chain.t * Id.t
  let compare (c1, n1) (c2, n2) =
    let c_cmp = compare c1 c2 in
    if c_cmp < 0 || c_cmp > 0 then c_cmp
    else compare n1 n2
end)

type t = {
  mutable active  : id list CMap.t;
  mutable blocks  : blocks CBMap.t;
  mutable branch  : branch CMap.t; (* highest branch on chain *)
  mutable chain   : chain;         (* hightest chain *)
  mutable height  : int CBMap.t;
  mutable sent    : messages CNMap.t;
  mutable sysmsgs : messages CMap.t;
}

let init () = {
  active  = CMap.(add (chain 1) [] empty);
  blocks  = CBMap.(add (chain 1, branch 0) Blocks.empty empty);
  branch  = CMap.(add (chain 1) (branch 0) empty);
  chain   = chain 1;
  height  = CBMap.(add (chain 1, branch 0) (-1) empty);
  sent    = CNMap.empty;
  sysmsgs = CMap.(add (chain 1) Messages.empty empty);
  }

(* active nodes on [chain] *)
let active info chain =
  try CMap.find chain info.active
  with Not_found -> []

(*  *)
let blocks info chain branch =
  try CBMap.find (chain, branch) info.blocks |> Blocks.to_list
  with Not_found -> []

(*  *)
let branch info chain =
  try CMap.find chain info.branch
  with Not_found -> branch (-1)

let branches info chain =
  let b = Branch.to_int (branch info chain) in
  let rec range a b =
    if a = b then [b] else a :: range (a + 1) b
  in
  if b < 0 then [] else List.map Basic.branch (range 0 b)

(*  *)
let chain info = info.chain

(*  *)
let height info chain branch =
  try CBMap.find (chain, branch) info.height
  with Not_found -> -1

let heights info chain branch =
  let h = height info chain branch in
  let rec range a b =
    if a = b then [b] else a :: range (a + 1) b
  in
  if h < 0 then [] else range 0 h
(*  *)
let sent info chain node =
  try CNMap.find (chain, node) info.sent |> Messages.to_list
  with Not_found -> []

(*  *)
let sysmsgs info chain =
  try CMap.find chain info.sysmsgs |> Messages.to_list
  with Not_found -> []

let chains info =
  let open Basic in
  let c = Chain.to_int info.chain in
  if c = 1 then [chain c]
  else
  let rec range a b = if a = b then [b] else a :: range (a + 1) b in
  List.map chain (range 1 c)

(* viewing functions *)
let view_active info chain =
  "[" ^ String.concat ", " (List.map Id.view (active info chain)) ^ "]"

let view_blocks info chain branch =
  "[" ^ String.concat ", " (List.map Block.view (blocks info chain branch)) ^ "]"

let view_branch info chain = Branch.to_int (branch info chain) |> string_of_int

let view_branches info chain =
  "[" ^ String.concat ", " (List.map Branch.view (branches info chain)) ^ "]"

let view_height info chain branch = height info chain branch |> string_of_int

let view_heights info chain branch =
  "[" ^ String.concat ", " (List.map string_of_int (heights info chain branch)) ^ "]"

let view_sent info chain node =
  "[" ^ String.concat ", " (List.map Message.view (sent info chain node)) ^ "]"

let view_sysmsgs info chain =
  "[" ^ String.concat ", " (List.map Message.view (sysmsgs info chain)) ^ "]"

(* view of a single [chain] *)
let view_chain info chain =
  let branch_id b = String.make 2 ' ' ^ Branch.view b ^ " :> " in
  let node_id n = String.make 2 ' ' ^ Id.view n ^ " :> " in
  String.concat "\n"
    [ "chain: " ^ Chain.view chain
    ; "active: " ^ view_active info chain
    ; "blocks:" ^ String.concat "\n"
      (let l = List.map (fun b -> branch_id b ^ view_blocks info chain b) (branches info chain) in
      if l = [] then l else "" :: l)
    ; "branch: " ^ view_branches info chain
    ; "height:" ^ String.concat "\n"
      (let l = List.map (fun b -> branch_id b ^ view_heights info chain b) (branches info chain) in
      if l = [] then l else "" :: l)
    ; "sent:" ^ String.concat "\n"
      (let l = List.map (fun n -> node_id n ^ view_sent info chain n) (active info chain) in
      if l = [] then l else "" :: l)
    ; "sysmsgs: " ^ view_sysmsgs info chain
    ]

(* view of complete state of the network *)
let view info =
  let chain_list = chains info in
  let chain_id c = String.make 2 ' ' ^ Chain.view c ^ " :> " in
  let branch_id b = String.make 4 ' ' ^ Branch.view b ^ " :> " in
  let node_id n = String.make 4 ' ' ^ Id.view n ^ " :> " in
  String.concat "\n"
    [ "chains: [" ^ String.concat ", " (List.map Chain.view chain_list) ^ "]"
    ; "active:\n" ^ String.concat "\n"
      (List.map (fun c -> chain_id c ^ view_active info c) chain_list)
    ; "blocks:\n" ^ String.concat "\n"
      (List.map (fun c ->
        chain_id c ^ String.concat "\n"
          (let l = List.map (fun b -> branch_id b ^ view_blocks info c b) (branches info c) in
          if l = [] then l else "" :: l))
      chain_list)
    ; "branch:\n" ^ String.concat "\n"
      (List.map (fun c -> chain_id c ^ view_branches info c) chain_list)
    ; "height:\n" ^ String.concat "\n"
      (List.map (fun c ->
        chain_id c ^ String.concat "\n"
          (let l = List.map (fun b -> branch_id b ^ view_heights info c b) (branches info c) in
          if l = [] then l else "" :: l))
      chain_list)
    ; "sent:\n" ^ String.concat "\n"
      (List.map (fun c ->
        chain_id c ^ String.concat "\n"
          (let l = List.map (fun n -> node_id n ^ view_sent info c n) (active info c) in
          if l = [] then l else "" :: l))
      chain_list)
    ; "sysmsgs:\n"^ String.concat "\n"
      (List.map (fun c -> chain_id c ^ view_sysmsgs info c) chain_list)
    ]
