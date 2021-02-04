(** String representation of network state *)

open Basic
open Type
open Printf

let view_list viewer l =
  sprintf "[%s]" @@ String.concat ", " @@ List.map viewer l

(* viewing functions *)
let view_active info chain = view_list Id.view @@ active info chain

let view_blocks info chain branch =
  view_list Block.view @@ blocks_list info chain branch

let view_branch info chain = Branch.view @@ current_branch info chain

let view_branches info chain =
  view_list Branch.view @@ List.sort Branch.compare @@ branches info chain

let view_chains info = view_list Chain.view @@ chains info

let view_nodes info = view_list Id.view @@ Array.to_list info.nodes

let view_height info chain branch =
  string_of_int @@ current_height info chain branch

let view_heights info chain branch =
  view_list string_of_int @@ heights info chain branch

let view_sent info chain node =
  view_list Message.view @@ sent_list info chain node

let view_sysmsgs info chain = view_list Message.view @@ sysmsgs_list info chain

let view_chain_blocks ?(sp = 2) info chain =
  let branch_id b = String.make sp ' ' ^ Branch.view b ^ " :> " in
  String.concat_endline
  @@ List.map (fun b -> branch_id b ^ view_blocks info chain b)
  @@ branches_with_blocks info chain

let view_chain_branches info chain =
  if branches info chain = [] then "" else view_branches info chain

let view_chain_heights ?(sp = 2) info chain =
  let branch_id b = String.make sp ' ' ^ Branch.view b ^ " :> " in
  String.concat_endline
  @@ List.map (fun b -> branch_id b ^ view_heights info chain b)
  @@ branches info chain

let view_chain_sent ?(sp = 2) info chain =
  let node_id n = String.make sp ' ' ^ Id.view n ^ " :> " in
  String.concat_endline
  @@ List.map (fun n -> node_id n ^ view_sent info chain n)
  @@ active info chain

let view_chain_sysmsgs info chain =
  let open Queue in
  let sysmsgs = sysmsgs info chain in
  if is_empty sysmsgs then "" else view_sysmsgs info chain

(* view of a single [chain] *)
let view_chain info chain =
  String.concat_endline
    [ "chain: " ^ Chain.view chain;
      "active: " ^ view_active info chain;
      "blocks:\n" ^ view_chain_blocks info chain;
      "branch: " ^ view_chain_branches info chain;
      "height:\n" ^ view_chain_heights info chain;
      "sent:\n" ^ view_chain_sent info chain;
      "sysmsgs: " ^ view_chain_sysmsgs info chain ]

let state_viewer info viewer some_chains =
  let chain_id c = String.make 2 ' ' ^ Chain.view c ^ " :> " in
  String.concat_endline
  @@ List.map (fun c -> chain_id c ^ viewer info c) some_chains

let view_state_active info =
  state_viewer info view_active @@ chains_with_active info

let view_state_blocks info =
  state_viewer info (view_chain_blocks ~sp:4) @@ chains_with_blocks info

let view_state_branch info =
  state_viewer info view_chain_branches @@ chains_with_branches info

let view_state_heights info =
  state_viewer info (view_chain_heights ~sp:4) @@ chains_with_height info

let view_state_sent info =
  state_viewer info (view_chain_sent ~sp:4) @@ chains_with_sent info

let view_state_sysmsgs info =
  state_viewer info view_chain_sysmsgs @@ chains_with_sysmsgs info

(* view of complete state of the network *)
let view info =
  String.concat_endline
    [ "chains: " ^ view_chains info;
      "nodes:  " ^ view_nodes info;
      "active:\n" ^ view_state_active info;
      "blocks:\n" ^ view_state_blocks info;
      "branch:\n" ^ view_state_branch info;
      "height:\n" ^ view_state_heights info;
      "sent:\n" ^ view_state_sent info;
      "sysmsgs:\n" ^ view_state_sysmsgs info ]

let view_trace t = Execution.view t.trace
