open Basic
open Type
open Printf

let sprintf_list l = sprintf "[%s]" @@ String.concat_comma l

let view_nodes info =
  let open Array in
  let str =
    fold_left
      (fun a b -> if a = "" then a ^ b else a ^ ", " ^ b)
      "" (map Id.view info.nodes)
  in
  sprintf "[%s]" str

let view_active info node =
  sprintf_list @@ List.map Chain.view @@ active info node

(** all active chains for all nodes *)
let view_all_active info =
  let node_id n = String.make 2 ' ' ^ Id.view n ^ " :> " in
  List.map (fun n -> node_id n ^ view_active info n) @@ nodes info
  |> String.concat_endline

let view_blocks info node chain branch =
  sprintf_list @@ List.map Block.view @@ blocks_list info node chain branch

let view_branches info node chain =
  sprintf_list @@ List.map Branch.view @@ branches info node chain

let view_chains info = Network_info.view_chains info.network

let view_expect info node chain =
  sprintf_list @@ List.map Message.view @@ expect_list info node chain

let view_headers info node chain =
  sprintf_list @@ List.map Header.view @@ headers_list info node chain

let view_heights info node chain =
  let view_heights_on_branch b =
    sprintf "%s :> %d" Branch.(view b) @@ height info node chain b
  in
  sprintf "(%s)" @@ String.concat_comma
  @@ List.map view_heights_on_branch
  @@ branches info node chain

let view_messages info node chain =
  sprintf_list @@ List.map Message.view @@ messages_list info node chain

(* node viewing *)
let node_viewer ?(sp = 2) info node viewer some_chains =
  let open String in
  let chain_id c = make sp ' ' ^ Chain.view c ^ " :> " in
  concat_endline
    (List.map
       (fun c ->
         if branches info node c = [] then ""
         else chain_id c ^ viewer info node c)
       some_chains)

let view_node_blocks ?(sp = 2) info node =
  let open String in
  let chain_id c = make sp ' ' ^ Chain.view c ^ " :> " in
  let branch_id b = make (sp + 2) ' ' ^ Branch.view b ^ " :> " in
  concat_endline
    (List.map (fun c ->
         chain_id c
         ^ concat_endline
             (List.map (fun b -> branch_id b ^ view_blocks info node c b)
             @@ branches_with_blocks info node c))
    @@ chains_with_blocks info node)

let view_node_branches ?(sp = 2) info node =
  node_viewer ~sp info node view_branches @@ chains_with_branches info node

let view_node_expect ?(sp = 2) info node =
  node_viewer ~sp info node view_expect @@ chains_with_expect info node

let view_node_headers ?(sp = 2) info node =
  node_viewer ~sp info node view_headers @@ chains_with_headers info node

let view_node_heights ?(sp = 2) info node =
  node_viewer ~sp info node view_heights @@ chains_with_height info node

let view_node_messages ?(sp = 2) info node =
  node_viewer ~sp info node view_messages @@ chains_with_msgs info node

(** view of [node]'s state *)
let view_node info node =
  String.concat_endline
    [ "node: " ^ Id.view node
    ; "active: " ^ view_active info node
    ; "blocks:\n" ^ view_node_blocks info node
    ; "branches:\n" ^ view_node_branches info node
    ; "expect:\n" ^ view_node_expect info node
    ; "headers:\n" ^ view_node_headers info node
    ; "height:\n" ^ view_node_heights info node
    ; "messages:\n" ^ view_node_messages info node
    ]

let print_node info = print (view_node info)

let view_node' info n = view_node info Id.(id n)

let print_node' info n = print_node info Id.(id n)

let state_viewer info viewer some_nodes =
  let node_id n = String.make 2 ' ' ^ Id.view n ^ " :> " in
  String.concat_endline
  @@ List.map (fun n -> node_id n ^ viewer info n) some_nodes

let view_state_blocks info =
  state_viewer info (view_node_blocks ~sp:4) @@ nodes_with_blocks info

let view_state_branches info =
  state_viewer info (view_node_branches ~sp:4) @@ nodes_with_branches info

let view_state_expect info =
  state_viewer info (view_node_expect ~sp:4) @@ nodes_with_expect info

let view_state_headers info =
  state_viewer info (view_node_headers ~sp:4) @@ nodes_with_headers info

let view_state_heights info =
  state_viewer info (view_node_heights ~sp:4) @@ nodes_with_height info

let view_state_messages info =
  state_viewer info (view_node_messages ~sp:4) @@ nodes_with_msgs info

(** view of complete state of all nodes *)
let view info =
  String.concat_endline
    [ "nodes:  " ^ view_nodes info
    ; "chains: " ^ view_chains info
    ; "active:\n" ^ view_all_active info
    ; "blocks:\n" ^ view_state_blocks info
    ; "branches:\n" ^ view_state_branches info
    ; "expect:\n" ^ view_state_expect info
    ; "headers:\n" ^ view_state_headers info
    ; "height:\n" ^ view_state_heights info
    ; "messages:\n" ^ view_state_messages info
    ]

let view_chain info = Network_info.view_chain info.network

let view_chain' info c = Network_info.view_chain info.network Chain.(id c)

let view_network info = Network_info.view info.network

let view_trace info = Network_info.view_trace info.network
