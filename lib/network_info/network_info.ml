(** State of the network *)

open Basic

(** parameters: chain *)
module CMap = Map.Make(Chain)

(** parameters: chain, branch *)
module CBMap = Map.Make(struct
  type t = Chain.t * Branch.t
  let compare (c1, b1) (c2, b2) =
    let c_cmp = compare c1 c2 in
    if c_cmp < 0 || c_cmp > 0 then c_cmp
    else compare b1 b2
end)

(** parameters: chain, node *)
module CNMap = Map.Make(struct
  type t = Chain.t * Id.t
  let compare (c1, n1) (c2, n2) =
    let c_cmp = compare c1 c2 in
    if c_cmp < 0 || c_cmp > 0 then c_cmp
    else compare n1 n2
end)

type t = {
  mutable active  : Id.t list CMap.t;
  mutable blocks  : Blocks.t CBMap.t;
  mutable branch  : Branch.t CMap.t; (** highest branch on chain *)
  mutable chain   : Chain.t;         (** hightest chain *)
  mutable height  : int CBMap.t;
  mutable sent    : Messages.t CNMap.t;
  mutable sysmsgs : Message.t Queue.t CMap.t;
}

let init () =
  let branch = Branch.id in
  let chain = Chain.id in
  { active  = CMap.(add (chain 1) [] empty)
  ; blocks  = CBMap.(add (chain 1, branch 0) Blocks.empty empty)
  ; branch  = CMap.(add (chain 1) (branch 0) empty)
  ; chain   = chain 1
  ; height  = CBMap.(add (chain 1, branch 0) (-1) empty)
  ; sent    = CNMap.empty
  ; sysmsgs = CMap.(add (chain 1) (Queue.create ()) empty)
  }

(** active nodes on [chain] *)
let active info chain =
  try CMap.find chain info.active
  with Not_found -> []

(** list of all blocks on [branch] of [chain] *)
let blocks info chain branch =
  try CBMap.find (chain, branch) info.blocks
  with Not_found -> Blocks.empty

let blocks_list info chain branch = blocks info chain branch |> Blocks.to_list

(** current branch on [chain] *)
let current_branch info chain =
  try CMap.find chain info.branch
  with Not_found -> Branch.id (-1)

(** list of all branches on [chain] *)
let branches info chain =
  let b = Branch.to_int (current_branch info chain) in
  let rec range a b =
    if a = b then [b] else a :: range (a + 1) b
  in
  if b < 0 then [] else List.map Branch.id (range 0 b)

(** height on [branch] of [chain] *)
let current_height info chain branch =
  try CBMap.find (chain, branch) info.height
  with Not_found -> -1

(** list of all heights on [branch] of [chain] *)
let heights info chain branch =
  let h = current_height info chain branch in
  let rec range a b =
    if a = b then [b] else a :: range (a + 1) b
  in
  if h < 0 then [] else range 0 h

(** list of messages sent to [node] on [chain], order is not important *)
let sent info chain node =
  try CNMap.find (chain, node) info.sent
  with Not_found -> Messages.empty

let sent_list info chain node = sent info chain node |> Messages.to_list

(** list of system messages on [chain], order is not important *)
let sysmsgs info chain =
  try CMap.find chain info.sysmsgs |> Queue.to_list
  with Not_found -> []

(** list of all chains *)
let chains info =
  let open Basic in
  let open Chain in
  let c = to_int info.chain in
  if c = 1 then [id c]
  else
  let rec range a b = if a = b then [b] else a :: range (a + 1) b in
  List.map id (range 1 c)

(* viewing functions *)
let view_active info chain =
  "[" ^ String.concat ", " (List.map Id.view (active info chain)) ^ "]"

let view_blocks info chain branch =
  "[" ^ String.concat ", " (List.map Block.view (blocks_list info chain branch)) ^ "]"

let view_branch info chain = Branch.to_int (current_branch info chain) |> string_of_int

let view_branches info chain =
  "[" ^ String.concat ", " (List.map Branch.view (branches info chain)) ^ "]"

let view_chains info =
  "[" ^ String.concat ", " (List.map Chain.view (chains info)) ^ "]"

let view_height info chain branch = current_height info chain branch |> string_of_int

let view_heights info chain branch =
  "[" ^ String.concat ", " (List.map string_of_int (heights info chain branch)) ^ "]"

let view_sent info chain node =
  "[" ^ String.concat ", " (List.map Message.view (sent_list info chain node)) ^ "]"

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

let check_chains info chain exp =
  if not (chain <= info.chain)
  then Printf.printf "%s\n"
    (Chain.view chain ^ " is not in the set of chains " ^ view_chains info)
  else exp

let check_active info chain node exp =
  check_chains info chain (
    if not (List.mem node (active info chain))
    then Printf.printf "node %s is not active on chain %s\n" Id.(view node) Chain.(view chain)
    else exp
  )

let check_branch_add info chain branch exp =
  check_chains info chain (
    if branch <= current_branch info chain
    then Printf.printf "branch %s already exists on chain %s\n" Branch.(view branch) Chain.(view chain)
    else exp
  )

let check_branch_exists info chain branch exp =
  check_chains info chain (
    if Branch.to_int branch < 0 || branch > current_branch info chain
    then Printf.printf "branch %s does not exist on chain %s\n" Branch.(view branch) Chain.(view chain)
    else exp
  )

let check_height_add info chain branch h exp =
  check_branch_exists info chain branch (
    let hgt = current_height info chain branch in
    if h <= hgt
    then Printf.printf "branch %s on chain %s is already at height %d which is higher than %d\n"
      Branch.(view branch) Chain.(view chain) hgt h
    else exp
  )

(* Block production *)
let produce_block info (block : Block.t) =
  let chain = block.header.chain in
  let branch = block.header.branch in
  let height = block.header.height in
  let exp_hgt = current_height info chain branch + 1 in
  check_height_add info chain branch height (
    if height <> exp_hgt
    then Printf.printf "The next block on chain %s branch %s must at height %d"
      Chain.(view chain) Branch.(view branch) exp_hgt
    else
    let blocks = Blocks.insert block (blocks info chain branch) in
    info.blocks <- CBMap.add (chain, branch) blocks info.blocks
  )

let produce_block' info c b height n =
  let block : Block.t = {
    header = { chain = Chain.id c; branch = Branch.id b; height};
    ops = (height, n) }
    in
    produce_block info block

(* randomize over chains, branches, num_ops, (height is determined) *)
let produce_block_random info =
  let c = Random.int Chain.(to_int info.chain) + 1 in
  let chain = Chain.id c in
  let b = Random.int Branch.(to_int (current_branch info chain) + 1) in
  let h = current_height info chain (Branch.id b) + 1 in
  let n = Random.int 10 in
  produce_block' info c b h n

(* Add new chain *)
let new_chain info =
  let c = Chain.to_int info.chain + 1 in
  info.chain <- Chain.id c

(* Add new branch on existing [chain] *)
let new_branch info chain =
  check_chains info chain (
    let b = Branch.(to_int (current_branch info chain) + 1 |> id) in
    info.branch <- CMap.add chain b info.branch
  )
