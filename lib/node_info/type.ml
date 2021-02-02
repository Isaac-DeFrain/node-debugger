[@@@warning "-32"]

open Basic

(** node ids *)
module IdMap = Map.Make (Id)

(** parameters: node, chain *)
module NCMap = Map.Make (struct
  type t = Id.t * Chain.t

  let compare (n1, c1) (n2, c2) =
    let n_cmp = compare n1 n2 in
    if n_cmp < 0 || n_cmp > 0 then n_cmp else compare c1 c2
end)

(** parameters: node, chain, branch *)
module NCBMap = Map.Make (struct
  type t = Id.t * Chain.t * Branch.t

  let compare (n1, c1, b1) (n2, c2, b2) =
    let n_cmp = compare n1 n2 in
    let c_cmp = compare c1 c2 in
    if n_cmp < 0 || n_cmp > 0 then n_cmp
    else if c_cmp < 0 || c_cmp > 0 then c_cmp
    else compare b1 b2
end)

(** complete state of all nodes *)
type t = {
  nodes : Id.t array;
  mutable active : Chain.t list IdMap.t;
  mutable blocks : Blocks.t NCBMap.t;
  mutable branches : Branch.t list NCMap.t;
  mutable expect : Messages.t NCMap.t;
  mutable headers : Headers.t NCMap.t;
  mutable height : int NCBMap.t;
  mutable messages : Message.t Queue.t NCMap.t;
  network : Network_info.t;
}

(* system will act as node 0 *)

let init num_nodes =
  let nodes = Array.init num_nodes (fun n -> Id.id @@ (n + 1)) in
  (* [| 1;...; num_nodes |] *)
  let sys_nodes = Array.init (num_nodes + 1) Id.id in
  (* [| 0;...; num_nodes |] *)
  {
    nodes;
    active = IdMap.empty;
    blocks = NCBMap.empty;
    branches = NCMap.empty;
    expect = NCMap.empty;
    headers = NCMap.empty;
    height = NCBMap.empty;
    messages = NCMap.empty;
    network = Network_info.init sys_nodes;
  }

let sys = Id.id 0

let nodes info = Array.to_list info.nodes

(** Node state accessors *)
let active info node = try IdMap.find node info.active with Not_found -> []

let active_nodes info = Network_info.active info.network

let chains info = Network_info.chains info.network

let inactive info chain =
  List.remove_list (active_nodes info chain) @@ nodes info

let nodes_active_on_some_chain info =
  List.(
    filter (fun n -> exists (fun c -> mem c @@ active info n) @@ chains info)
    @@ nodes info)

let nodes_inactive_on_some_chain info =
  List.(
    filter (fun n ->
        exists (fun c -> mem n @@ inactive info c)
        @@ Network_info.chains info.network)
    @@ nodes info)

let inactive_chains info node =
  List.remove_list (active info node) @@ Network_info.chains info.network

let branches info node chain =
  try NCMap.find (node, chain) info.branches with Not_found -> []

let blocks info node chain branch =
  try NCBMap.find (node, chain, branch) info.blocks
  with Not_found -> Blocks.empty

let blocks_list info node chain branch =
  blocks info node chain branch |> Blocks.to_list

let has_block_on_branch info node chain branch =
  blocks info node chain branch <> Blocks.empty

let has_block_on_chain info node chain =
  List.exists (has_block_on_branch info node chain) @@ branches info node chain

let has_block info node =
  List.exists (has_block_on_chain info node) @@ chains info

let nodes_with_blocks info = List.filter (has_block info) @@ nodes info

let chains_with_blocks info node =
  List.filter (has_block_on_chain info node) @@ chains info

let branches_with_blocks info node chain =
  List.filter (has_block_on_branch info node chain) @@ branches info node chain

let block_at_height_on info node chain branch height =
  try
    let open List in
    Some
      ( hd
      @@ filter (fun (b : Block.t) -> b.header.height = height)
      @@ blocks_list info node chain branch )
  with Failure _ -> None

let has_block_at_height info node chain branch height =
  Option.is_some @@ block_at_height_on info node chain branch height

let has_branch_on_chain info node chain = branches info node chain <> []

let chains_with_branches info node =
  List.filter (has_branch_on_chain info node) @@ chains info

let has_branches info node =
  List.exists (has_branch_on_chain info node) @@ chains info

let nodes_with_branches info = List.filter (has_branches info) @@ nodes info

let chains_with_branches info node =
  List.filter (has_branch_on_chain info node) @@ chains info

let current_branch info node chain =
  match branches info node chain with [] -> Branch.id (-1) | hd :: _ -> hd

let expect info node chain =
  try NCMap.find (node, chain) info.expect with Not_found -> Messages.empty

let expect_list info node chain = expect info node chain |> Messages.to_list

let has_expect_on_chain info node chain =
  expect info node chain <> Messages.empty

let has_expect info node =
  List.exists (has_expect_on_chain info node) @@ chains info

let nodes_with_expect info = List.filter (has_expect info) @@ nodes info

let chains_with_expect info node =
  List.filter (has_expect_on_chain info node) @@ chains info

let headers info node chain =
  try NCMap.find (node, chain) info.headers with Not_found -> Headers.empty

let headers_list info node chain = headers info node chain |> Headers.to_list

let has_headers_on_chain info node chain =
  headers info node chain <> Headers.empty

let has_headers info node =
  List.exists (has_headers_on_chain info node) @@ chains info

let nodes_with_headers info = List.filter (has_headers info) @@ nodes info

let chains_with_headers info node =
  List.filter (has_headers_on_chain info node) @@ chains info

let height info node chain branch =
  try NCBMap.find (node, chain, branch) info.height with Not_found -> -1

let has_height_on_branch info node chain branch =
  height info node chain branch <> -1

let has_height_on_chain info node chain =
  List.exists (has_height_on_branch info node chain)
  @@ branches info node chain

let has_height info node =
  List.exists (has_headers_on_chain info node) @@ chains info

let nodes_with_height info = List.filter (has_height info) @@ nodes info

let chains_with_height info node =
  List.filter (has_height_on_chain info node) @@ chains info

let messages info node chain =
  try NCMap.find (node, chain) info.messages
  with Not_found -> Queue.create ()

let messages_list info node chain = messages info node chain |> Queue.to_list

let has_msgs_on_chain info node chain =
  not (Queue.is_empty @@ messages info node chain)

let has_msgs info node =
  List.exists (has_msgs_on_chain info node) @@ chains info

let nodes_with_msgs info = List.filter (has_msgs info) @@ nodes info

let chains_with_msgs info node =
  List.filter (has_msgs_on_chain info node) @@ chains info

let peek info node chain = Queue.peek @@ messages info node chain

let peek_opt info node chain = Queue.peek_opt @@ messages info node chain

(** Network state accessors *)
let sent info = Network_info.sent info.network

let sysmsgs info = Network_info.sysmsgs info.network
