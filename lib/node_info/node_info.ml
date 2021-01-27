[@@@warning "-27"]

open Basic

(** node ids *)
module IdMap = Map.Make(Id)

(** parameters: node, chain *)
module NCMap = Map.Make(struct
  type t = Id.t * Chain.t
  let compare (n1, c1) (n2, c2) =
    let n_cmp = compare n1 n2 in
    if n_cmp < 0 || n_cmp > 0 then n_cmp
    else compare c1 c2
end)

(** parameters: node, chain, branch *)
module NCBMap = Map.Make(struct
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
  mutable active   : Chain.t list IdMap.t;
  mutable blocks   : Blocks.t NCBMap.t;
  mutable branches : Branch.t list NCMap.t;
  mutable expect   : Messages.t NCMap.t;
  mutable headers  : Headers.t NCMap.t;
  mutable height   : int NCBMap.t;
  mutable messages : Message.t Queue.t NCMap.t;
}

let init num_nodes =
  let nodes = Array.init num_nodes Id.id in
  {
    nodes;
    active   = IdMap.empty;
    blocks   = NCBMap.empty;
    branches = NCMap.empty;
    expect   = NCMap.empty;
    headers  = NCMap.empty;
    height   = NCBMap.empty;
    messages = NCMap.empty;
}

(* viewing functions *)
let active info node =
  try IdMap.find node info.active
  with Not_found -> []

let blocks info node chain branch =
  try NCBMap.find (node, chain, branch) info.blocks |> Blocks.to_list
  with Not_found -> []

let branches info node chain =
  try NCMap.find (node, chain) info.branches
  with Not_found -> []

let current_branch info node chain : Branch.t =
  match branches info node chain with
  | [] -> Branch.id (-1)
  | hd::_ -> hd

let expect info node chain =
  try NCMap.find (node, chain) info.expect |> Messages.to_list
  with Not_found -> []

let headers info node chain =
  try NCMap.find (node, chain) info.headers |> Headers.to_list
  with Not_found -> []

let height info node chain branch =
  try NCBMap.find (node, chain, branch) info.height
  with Not_found -> -1

let messages info node chain =
  try NCMap.find (node, chain) info.messages |> Queue.to_list
  with Not_found -> []

let view_nodes info =
  "[" ^ Array.fold_left (fun a b -> if a = "" then a ^ b else a ^ ", " ^ b) ""
    (Array.map Id.view info.nodes) ^ "]"

let view_active info node =
  "[" ^ String.concat ", " (List.map Chain.view (active info node)) ^ "]"

let view_blocks info node chain branch =
  "[" ^ String.concat ", " (List.map Block.view (blocks info node chain branch)) ^ "]"

let view_branches info node chain =
  "[" ^ String.concat ", " (List.map Branch.view (branches info node chain)) ^ "]"

let view_expect info node chain =
  "[" ^ String.concat ", "
  (List.map Message.view (expect info node chain)) ^ "]"

let view_headers info node chain =
  "[" ^ String.concat ", "
  (List.map Header.view (headers info node chain)) ^ "]"

let view_heights info node chain =
  "(" ^ String.concat ", "
  (List.map (fun b -> (Branch.view b ^ " :> " ^ string_of_int (height info node chain b)))
    (branches info node chain)) ^ ")"

let view_messages info node chain =
  "[" ^ String.concat ", " (List.map Message.view (messages info node chain)) ^ "]"

(** view of [node]'s state *)
let view_node info node =
  let chain_id c = String.make 2 ' ' ^ Chain.view c ^ " :> " in
  let branch_id b = String.make 4 ' ' ^ Branch.view b ^ " :> " in
  String.concat "\n"
    [ "node: " ^ Id.view node
    ; "active: " ^ view_active info node
    ; "blocks:\n" ^ String.concat "\n"
      (List.map (fun c ->
        chain_id c ^ String.concat "\n"
        (let l = List.map (fun b -> branch_id b ^ view_blocks info node c b) (branches info node c) in
        if l = [] then l else "" :: List.rev l))
      (active info node))
    ; "branches:\n" ^ String.concat "\n"
      (List.map (fun c -> chain_id c ^ view_branches info node c) (active info node))
    ; "expect:\n" ^ String.concat "\n"
      (List.map (fun c -> chain_id c ^ view_expect info node c) (active info node))
    ; "headers:\n" ^ String.concat "\n"
      (List.map (fun c -> chain_id c ^ view_headers info node c) (active info node))
    ; "height:\n" ^ String.concat "\n"
      (List.map (fun c -> chain_id c ^ view_heights info node c) (active info node))
    ; "messages:\n" ^ String.concat "\n"
      (List.map (fun c -> chain_id c ^ view_messages info node c) (active info node))
    ]

(** view of complete state of all nodes *)
let view info =
  let node_id n = String.make 2 ' ' ^ Id.view n ^ " :> " in
  let chain_id c = String.make 4 ' ' ^ Chain.view c ^ " :> " in
  let branch_id b = String.make 6 ' ' ^ Branch.view b ^ " :> " in
  String.concat "\n"
    [ "nodes: " ^ view_nodes info
    ; "active:\n" ^ String.concat "\n"
        (Array.to_list (Array.map (fun n -> node_id n ^ view_active info n)
        info.nodes))
    ; "blocks:\n" ^ String.concat "\n"
        (Array.to_list (Array.map (fun n ->
          node_id n ^ String.concat "\n"
            (List.map (fun c ->
              chain_id c ^ String.concat "\n"
                (let l = List.map (fun b ->
                  branch_id b ^ view_blocks info n c b) (branches info n c)
                in
                if l = [] then l else "" :: List.rev l))
            (active info n)))
        info.nodes))
    ; "branches:\n" ^ String.concat "\n"
        (Array.to_list (Array.map (fun n ->
          node_id n ^ String.concat "\n"
          (List.map (fun c -> chain_id c ^ view_branches info n c) (active info n)))
        info.nodes))
    ; "expect:\n" ^ String.concat "\n"
        (Array.to_list (Array.map (fun n ->
          node_id n ^ String.concat "\n"
          (List.map (fun c -> chain_id c ^ view_expect info n c) (active info n)))
        info.nodes))
    ; "headers:\n" ^ String.concat "\n"
        (Array.to_list (Array.map (fun n ->
          node_id n ^ String.concat "\n"
            (List.map (fun c -> chain_id c ^ view_headers info n c) (active info n)))
        info.nodes))
    ; "height:\n" ^ String.concat "\n"
        (Array.to_list (Array.map (fun n ->
          node_id n ^ String.concat "\n"
            (List.map (fun c -> chain_id c ^ view_heights info n c) (active info n)))
        info.nodes))
    ; "messages:\n" ^ String.concat "\n"
        (Array.to_list (Array.map (fun n ->
          node_id n ^ String.concat "\n"
          (List.map (fun c -> chain_id c ^ view_messages info n c) (active info n)))
        info.nodes))
    ]

(* Node actions *)
let not_active node chain =
  Printf.printf "node %s is not active on chain %s\n" Id.(view node) Chain.(view chain)

let already_active node chain =
  Printf.printf "node %s is already active on chain %s\n" Id.(view node) Chain.(view chain)

let check_nodes info node exp =
  if not (Array.mem node info.nodes)
  then Printf.printf "node %s is not in the set of nodes: %s" Id.(view node) (view_nodes info)
  else exp

let check_active info node chain exp =
  if not (List.mem chain (active info node))
  then not_active node chain
  else exp

(* Activate/Deactivate *)
(** [node] becomes active on [chain] *)
let activate info node chain =
  check_nodes info node (
    match IdMap.find_opt node info.active with
    | None -> info.active <- IdMap.add node [chain] info.active
    | Some chains ->
      if List.mem chain chains
      then already_active node chain
      else
      let updated = List.sort_uniq compare (chain :: chains) in
      info.active <- IdMap.add node updated info.active)

(** [node] becomes inactive on [chain] *)
let deactivate info node chain =
  check_nodes info node (
    match IdMap.find_opt node info.active with
    | None -> already_active node chain
    | Some chains ->
      if not (List.mem chain chains)
      then already_active node chain
      else
      let updated = List.remove chain chains in
      info.active <- IdMap.add node updated info.active)

(** [node] receives a specific [msg] on [chain] *)
let receive_msg info network node chain msg =
  check_nodes info node (
    check_active info node chain (
      let open Network_info in
      match sent_list network chain node with
      | [] ->
        Printf.printf "node %s has no messages on chain %s\n" Id.(view node) Chain.(view chain)
      | msgs ->
        if not (List.mem msg msgs)
        then Printf.printf
          "The message\n  %s\nwas not found in node %s's sent messages on chain %s:\n  %s"
          Message.(view msg) Id.(view node) Chain.(view chain) Messages.(of_list msgs |> view)
        else
        let net_msgs = List.remove msg msgs |> Messages.of_list in
        let node_msgs =
          let open Queue in
          let m = messages info node chain |> of_list in
          push msg m; m
        in
        (* remove [msg] from network_info.sent *)
        network.sent <- CNMap.add (chain, node) net_msgs network.sent;
        (* add [msg] to node_info.messages *)
        info.messages <- NCMap.add (node, chain) node_msgs info.messages
    )
  )

(* sending messages *)
open Msg
let send_ack info network sender receiver chain ack =
  check_nodes info sender (
    check_active info sender chain (
      let open Network_info in
      let ack' = Message.Msg (sender, receiver, Ack ack) in
      let updated = sent network chain receiver |> Messages.add ack' in
      (* add ack to network_info.sent *)
      network.sent <- CNMap.add (chain, receiver) updated network.sent
    )
  )

let send_advertise info network sender receiver chain adv =
  check_nodes info sender (
    check_active info sender chain (
      let open Network_info in
      let adv' = Message.Msg (sender, receiver, Adv adv) in
      let updated = sent network chain receiver |> Messages.add adv' in
      (* add ack to network_info.sent *)
      network.sent <- CNMap.add (chain, receiver) updated network.sent
    )
  )

let send_request info node chain (* req_type *) =
  check_nodes info node ()

let send_err info node chain (* err_type *) =
  check_nodes info node ()

(** [node] updates branches on [chain] *)
let update_branch info node chain branch =
  check_nodes info node (
    let bs = branches info node chain in
    if List.mem branch bs
    then Printf.printf "node %s alreday knows about branch %s on chain %s\n"
      Id.(view node) Branch.(view branch) Chain.(view chain)
    else
    let updated = List.sort_uniq Branch.compare (branch :: bs) in
    info.branches <- NCMap.add (node, chain) updated info.branches)

(** [node] updates height on [branch] of [chain] *)
let update_height info node chain branch h =
  check_nodes info node (
    if h <= height info node chain branch
    then Printf.printf "node %s already knows about a block at height %d on branch %s of chain %s\n"
      Id.(view node) h Branch.(view branch) Chain.(view chain)
    else
    info.height <- NCBMap.add (node, chain, branch) h info.height)

(** [node] handles a message on [chain] *)
let handle info node chain msg =
  check_nodes info node ()

(* [sndr] advertises current branch to [rcvr] on [chain] *)
let advertise_branch_one info network sndr rcvr chain =
  check_nodes info sndr (
    let branch = current_branch info sndr chain in
    let open Network_info in
    let adv = Message.Msg (sndr, rcvr, Adv (Adv_current_branch (chain, branch))) in
    let updated = sent network chain rcvr |> Messages.add adv in
    (* [adv] sent to [rcvr] *)
    network.sent <- CNMap.add (chain, rcvr) updated network.sent;
    (* register expectation *)
    ()
  )
