open Basic
open Type
open Printf

let view_nodes info =
  let open Array in
  let str =
    fold_left
      (fun a b -> if a = "" then a ^ b else a ^ ", " ^ b)
      ""
      (map Id.view info.nodes)
  in
  sprintf "[%s]" str

let view_active info node =
  "[" ^ String.concat ", " (List.map Chain.view (active info node)) ^ "]"

let view_actives info =
  let open Array in
  let node_id n = String.make 2 ' ' ^ Id.view n ^ " :> " in
  to_list (map (fun n -> node_id n ^ view_active info n) info.nodes)
  |> String.concat_endline

let view_blocks info node chain branch =
  "["
  ^ String.concat
      ", "
      (List.map Block.view (blocks_list info node chain branch))
  ^ "]"

let view_branches info node chain =
  "["
  ^ String.concat ", " (List.map Branch.view (branches info node chain))
  ^ "]"

let view_chains info = Network_info.view_chains info.network

let view_expect info node chain =
  let str =
    String.concat ", " (List.map Message.view (expect_list info node chain))
  in
  sprintf "[%s]" str

let view_headers info node chain =
  let str =
    String.concat ", " (List.map Header.view (headers info node chain))
  in
  sprintf "[%s]" str

let view_heights info node chain =
  let view_heights_on_branch b =
    sprintf "%s :> %d" Branch.(view b) (height info node chain b)
  in
  let str =
    String.concat
      ", "
      (List.map view_heights_on_branch (branches info node chain))
  in
  sprintf "(%s)" str

let view_messages info node chain =
  "["
  ^ String.concat ", " (List.map Message.view (messages_list info node chain))
  ^ "]"

(* node viewing *)
let node_viewer ?(sp = 2) info node viewer =
  let chain_id c = String.make sp ' ' ^ Chain.view c ^ " :> " in
  String.concat_endline
    (List.map
       (fun c ->
         if branches info node c = [] then ""
         else chain_id c ^ viewer info node c)
       (active info node))

let view_node_blocks ?(sp = 2) info node =
  let chain_id c = String.make sp ' ' ^ Chain.view c ^ " :> " in
  let branch_id b = String.make (sp + 2) ' ' ^ Branch.view b ^ " :> " in
  String.concat_endline
    (List.map
       (fun c ->
         chain_id c
         ^ String.concat_endline
             (List.map
                (fun b ->
                  if blocks info node c b = Blocks.empty then ""
                  else branch_id b ^ view_blocks info node c b)
                (branches info node c)))
       (active info node))

let view_node_branches ?(sp = 2) info node =
  node_viewer ~sp info node view_branches

let view_node_expect ?(sp = 2) info node =
  node_viewer ~sp info node view_expect

let view_node_headers ?(sp = 2) info node =
  node_viewer ~sp info node view_headers

let view_node_heights ?(sp = 2) info node =
  node_viewer ~sp info node view_heights

let view_node_messages ?(sp = 2) info node =
  node_viewer ~sp info node view_messages

(** view of [node]'s state *)
let view_node info node =
  String.concat_endline
    [ "node: " ^ Id.view node;
      "active: " ^ view_active info node;
      "blocks:\n" ^ view_node_blocks info node;
      "branches:\n" ^ view_node_branches info node;
      "expect:\n" ^ view_node_expect info node;
      "headers:\n" ^ view_node_headers info node;
      "height:\n" ^ view_node_heights info node;
      "messages:\n" ^ view_node_messages info node ]

let state_viewer info viewer =
  let node_id n = String.make 2 ' ' ^ Id.view n ^ " :> " in
  String.concat_endline
    Array.(to_list (map (fun n -> node_id n ^ viewer info n) info.nodes))

let view_state_blocks info = state_viewer info (view_node_blocks ~sp:4)

let view_state_branches info = state_viewer info (view_node_branches ~sp:4)

let view_state_expect info = state_viewer info (view_node_expect ~sp:4)

let view_state_headers info = state_viewer info (view_node_headers ~sp:4)

let view_state_heights info = state_viewer info (view_node_heights ~sp:4)

let view_state_messages info = state_viewer info (view_node_messages ~sp:4)

(** view of complete state of all nodes *)
let view info =
  String.concat_endline
    [ "nodes:  " ^ view_nodes info;
      "chains: " ^ view_chains info;
      "active:\n" ^ view_actives info;
      "blocks:\n" ^ view_state_blocks info;
      "branches:\n" ^ view_state_branches info;
      "expect:\n" ^ view_state_expect info;
      "headers:\n" ^ view_state_headers info;
      "height:\n" ^ view_state_heights info;
      "messages:\n" ^ view_state_messages info ]

let view_network info = Network_info.view info.network

let view_trace info = Network_info.view_trace info.network
