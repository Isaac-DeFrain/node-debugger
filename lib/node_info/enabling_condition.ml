[@@@warning "-33"]

[@@@warning "-27"]

(** {1 Enabling conditions for node & system actions} *)

open Basic
open Type
open View
open Printf

(** {2 System actions} *)
let view_new_chain _ = "New_chain"

let view_new_branch _ = "New_branch"

let view_new_block _ = "New_block"

let sprintf_c = sprintf "    chain(s) %s"

let sprintf_cb chain =
  sprintf "    chain %s :> branch(es) %s" Chain.(view chain)

let chains_with_active_node info =
  List.(filter ~f:(fun c -> remove_all sys @@ active_nodes info c <> []))
  @@ chains info

(** {3 Viewing functions} *)

let view_advertise_curr_branch_sys info =
  let active_chains = chains_with_active_node info in
  if active_chains = [] then ""
  else
    sprintf "Advertise: Current_branch on chain(s) %s"
    @@ String.concat_comma
    @@ List.map ~f:Chain.view active_chains

let view_advertise_curr_head_sys info =
  let active_chains = chains_with_active_node info in
  if active_chains = [] then ""
  else
    "Advertise: Current_head\n" ^ String.concat_endline
    @@ List.(
         map ~f:(fun c ->
             sprintf_cb c @@ String.concat_comma @@ map ~f:Branch.view
             @@ Network_info.branches info.network c))
         active_chains

(* TODO advertise Block_header, Operations *)
(* for Advertise_sys Block_header to be enabled, sys must have a request for
   that header as the next sysmsgs *)

(* for Advertise_sys Operations to be enabled, sys must have a request for those
   ops as the next sysmsgs *)

let receive_sys info chain = sent info chain sys <> Messages.empty

let view_receive_sys info =
  let recv_chains = List.filter ~f:(receive_sys info) @@ chains info in
  if recv_chains = [] then ""
  else
    "Receive_sys\n" ^ sprintf_c @@ String.concat_comma
    @@ List.(map ~f:Chain.view @@ filter ~f:(receive_sys info) recv_chains)

let handle_sys info chain = not (Queue.is_empty @@ sysmsgs info chain)

let view_handle_sys info =
  let hdl_chains = List.filter ~f:(handle_sys info) @@ chains info in
  if hdl_chains = [] then ""
  else
    "Handle_sys\n" ^ sprintf_c @@ String.concat_comma
    @@ List.(map ~f:Chain.view @@ filter ~f:(handle_sys info) hdl_chains)

(** {2 Node actions} *)

let sprintf_nc node = sprintf "    node %s :> chain(s) %s" Id.(view node)

let activate info node chain =
  not (List.mem (active info node) chain ~equal:Chain.equal)

(* returns inactive nodes and which chains they are inactive on *)
(* these are all the nodes that can do an activate action *)
let view_activate info =
  let inactive_some = nodes_inactive_on_some_chain info in
  if inactive_some = [] then ""
  else
    "Activate\n" ^ String.concat_endline
    @@ List.(
         map ~f:(fun n ->
             sprintf_nc n @@ String.concat_comma @@ map ~f:Chain.view
             @@ inactive_chains info n))
         inactive_some

let deactivate info node chain = not (activate info node chain)

let view_deactivate info =
  let active_some = nodes_active_on_some_chain info in
  if active_some = [] then ""
  else
    "Deactivate\n" ^ String.concat_endline
    @@ List.(
         map ~f:(fun n ->
             sprintf_nc n @@ String.concat_comma @@ map ~f:Chain.view
             @@ active info n))
         active_some

let receive info node chain = sent info chain node <> Messages.empty

let chains_with_msgs_to_recv info node =
  List.filter ~f:(receive info node) @@ active info node

let nodes_with_msg_to_recv info =
  List.filter ~f:(fun n -> chains_with_msgs_to_recv info n <> []) @@ nodes info

let view_receive info =
  let msg_nodes = nodes_with_msg_to_recv info in
  if msg_nodes = [] then ""
  else
    "Receive\n" ^ String.concat_endline
    @@ List.(
         map ~f:(fun n ->
             sprintf_nc n @@ String.concat_comma @@ map ~f:Chain.view
             @@ chains_with_msgs_to_recv info n))
         msg_nodes

let handle info node chain = not (Queue.is_empty @@ messages info node chain)

let chains_with_msg_to_hdl info node =
  List.filter ~f:(handle info node) @@ active info node

let nodes_with_msg_to_hdl info =
  List.(
    filter ~f:(fun n -> exists ~f:(fun c -> handle info n c) @@ chains info))
  @@ nodes_active_on_some_chain info

let view_handle info =
  let hdl_nodes = nodes_with_msg_to_hdl info in
  if hdl_nodes = [] then ""
  else
    "Handle\n" ^ String.concat_endline
    @@ List.(
         map ~f:(fun n ->
             sprintf_nc n @@ String.concat_comma @@ map ~f:Chain.view
             @@ chains_with_msg_to_hdl info n))
         hdl_nodes

(* TODO finish enabling conditions for node actions *)

let eval_all x = function
  | [] -> []
  | fs ->
    let rec aux x acc = function
      | [] -> List.rev acc
      | hd :: tl -> aux x (hd x :: acc) tl
    in
    aux x [] fs

(** {2 Viewing enabling conditions} *)
let view_enabled_sys info =
  eval_all info
    [ view_new_chain
    ; view_new_branch
    ; view_new_block
    ; view_receive_sys
    ; view_handle_sys
    ; view_advertise_curr_branch_sys
    ; view_advertise_curr_head_sys
    ]
  |> List.remove_all ""

let view_enabled_node info =
  eval_all info [ view_activate; view_deactivate; view_receive; view_handle ]
  |> fun l -> List.remove_all "" l @ [ "" ]

open Execution.Action

(** {1 Enabling conditions for actions} *)

let rec enabled info = function
  | Node node_action -> enabled_node info node_action
  | Sys sys_action -> enabled_sys info sys_action

and enabled_node info = function
  | Activate (n, c) -> activate info n c
  | Deactivate (n, c) -> deactivate info n c
  | Recv_node (n, c, _) -> receive info n c
  | Ack (n, c, _)
  | Adv_one (n, _, c, _)
  | Adv_all (n, c, _, _)
  | Err (n, c, _)
  | Req_one (n, _, c, _)
  | Req_all (n, c, _)
  | Handle_ack (n, c, _)
  | Handle_adv (n, c, _)
  | Handle_err (n, c, _)
  | Handle_req (n, c, _) -> handle info n c
  | Update_branch (n, c, _) ->
    raise @@ Failure "TODO Update_branch enabled_node"
  | Update_height (n, c, _, _) ->
    raise @@ Failure "TODO Update_height enabled_node"
  | Update_headers (n, c, _) ->
    raise @@ Failure "TODO Update_headers enabled_node"
  | Update_blocks (n, c, _) ->
    raise @@ Failure "TODO Update_blocks enabled_node"

and enabled_sys info = function
  | New_chain _ | New_block _ | New_branch _ -> true
  | Recv_sys (c, _) -> receive_sys info c
  | Adv_one_sys (_, c, _) | Adv_all_sys (c, _) -> handle_sys info c
