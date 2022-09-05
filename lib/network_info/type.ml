(** Type of network state *)

open Basic

(** parameters: chain *)
module CMap = Map.Make (Chain)

(** parameters: chain, branch *)
module CBMap = Map.Make (struct
  type t = Chain.t * Branch.t

  let compare (c1, b1) (c2, b2) =
    let c_cmp = compare c1 c2 in
    if c_cmp < 0 || c_cmp > 0 then c_cmp else compare b1 b2
end)

(** parameters: chain, node *)
module CNMap = Map.Make (struct
  type t = Chain.t * Id.t

  let compare (c1, n1) (c2, n2) =
    let c_cmp = compare c1 c2 in
    if c_cmp < 0 || c_cmp > 0 then c_cmp else compare n1 n2
end)

type t =
  { mutable active : Id.t list CMap.t
  ; mutable blocks : Blocks.t CBMap.t
  ; mutable branch : Branch.t CMap.t  (** highest branch on chain *)
  ; mutable chain : Chain.t  (** hightest chain *)
  ; mutable height : int CBMap.t
  ; mutable sent : Messages.t CNMap.t
  ; mutable sysmsgs : Message.t Queue.t CMap.t
  ; trace : Execution.t
  ; nodes : Id.t array
  }

(* chain 1 and branch 0 exist upon creation *)
let init nodes =
  let branch = Branch.id 0 in
  let chain = Chain.id 1 in
  { active = CMap.(add chain [] empty)
  ; blocks = CBMap.(add (chain, branch) Blocks.empty empty)
  ; branch = CMap.(add chain branch empty)
  ; chain
  ; height = CBMap.(add (chain, branch) (-1) empty)
  ; sent = CNMap.empty
  ; sysmsgs = CMap.(add chain (Queue.create ()) empty)
  ; trace = Execution.init ()
  ; nodes
  }

(** [sys] is node 0 *)
let sys = Id.id 0

(** active nodes on [chain] *)
let active info chain = try CMap.find chain info.active with Not_found -> []

(** all blocks on [branch] of [chain] *)
let blocks info chain branch =
  try CBMap.find (chain, branch) info.blocks with Not_found -> Blocks.empty

let blocks_list info chain branch = blocks info chain branch |> Blocks.to_list

(** current branch on [chain] *)
let current_branch info chain =
  try CMap.find chain info.branch with Not_found -> Branch.id (-1)

(** list of all branches on [chain] *)
let branches info chain =
  let open List in
  let b = Branch.to_int (current_branch info chain) in
  let rec range a b acc =
    if b < 0 then [] else if a > b then rev acc else range (a + 1) b (a :: acc)
  in
  map ~f:Branch.id (range 0 b [])

(** height on [branch] of [chain] *)
let current_height info chain branch =
  try CBMap.find (chain, branch) info.height with Not_found -> -1

(** list of all heights on [branch] of [chain] *)
let heights info chain branch =
  let h = current_height info chain branch in
  let rec range a b = if a = b then [ b ] else a :: range (a + 1) b in
  if h < 0 then [] else range 0 h

(** list of messages sent to [node] on [chain], order is not important *)
let sent info chain node =
  try CNMap.find (chain, node) info.sent with Not_found -> Messages.empty

let sent_list info chain node = sent info chain node |> Messages.to_list

let sent_sys info chain = sent info chain sys

(** list of system messages on [chain], order is not important *)
let sysmsgs info chain =
  try CMap.find chain info.sysmsgs with Not_found -> Queue.create ()

let sysmsgs_list info chain = sysmsgs info chain |> Queue.to_list

(** list of all chains *)
let chains info =
  let open Chain in
  let c = to_int info.chain in
  if c = 1 then [ id c ]
  else
    let rec range a b = if a = b then [ b ] else a :: range (a + 1) b in
    List.map ~f:id (range 1 c)

(** list of all nodes *)
let nodes info = Array.to_list info.nodes

let has_active_on_chain info chain = active info chain <> []

let chains_with_active info =
  List.filter ~f:(has_active_on_chain info) @@ chains info

let has_blocks_on_branch info chain branch =
  blocks info chain branch <> Blocks.empty

let has_blocks_on_chain info chain =
  List.exists ~f:(has_blocks_on_branch info chain) @@ branches info chain

let chains_with_blocks info =
  List.filter ~f:(has_blocks_on_chain info) @@ chains info

let branches_with_blocks info chain =
  List.filter ~f:(has_blocks_on_branch info chain) @@ branches info chain

let has_branches_on_chain info chain = branches info chain <> []

let chains_with_branches info =
  List.filter ~f:(has_branches_on_chain info) @@ chains info

let has_height_on_branch info chain branch =
  current_height info chain branch >= 0

let has_height_on_chain info chain =
  List.exists ~f:(has_height_on_branch info chain) @@ branches info chain

let chains_with_height info =
  List.filter ~f:(has_height_on_chain info) @@ chains info

let has_sent_by_node info chain node = sent info chain node <> Messages.empty

let has_sent_on_chain info chain =
  List.exists ~f:(has_sent_by_node info chain) @@ nodes info

let chains_with_sent info =
  List.filter ~f:(has_sent_on_chain info) @@ chains info

let has_sysmsgs_on_chain info chain = not (Queue.is_empty @@ sysmsgs info chain)

let chains_with_sysmsgs info =
  List.filter ~f:(has_sysmsgs_on_chain info) @@ chains info
