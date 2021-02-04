(** Unit tests *)

open Basic
open Node_info
open Utils

(** comparing states *)
let all_true l = List.(for_all (fun b -> b) @@ map snd l)

let full_eq state1 state2 =
  [ ("active", state1.active = state2.active);
    ("blocks", state1.blocks = state2.blocks);
    ("branches", state1.branches = state2.branches);
    ("expect", state1.expect = state2.expect);
    ("headers", state1.headers = state2.headers);
    ("height", state1.height = state2.height);
    ("messages", state1.messages = state2.messages);
    ("net_active", state1.network.active = state2.network.active);
    ("net_blocks", state1.network.blocks = state2.network.blocks);
    ("net_branch", state1.network.branch = state2.network.branch);
    ("net_chain", state1.network.chain = state2.network.chain);
    ("net_height", state1.network.height = state2.network.height);
    ("net_blocks", state1.network.blocks = state2.network.blocks);
    ("net_sent", state1.network.sent = state2.network.sent);
    ("net_sysmsgs", state1.network.sysmsgs = state2.network.sysmsgs) ]

let equal_except_active state1 state2 =
  all_true @@ List.remove_assoc "active" @@ full_eq state1 state2

let equal_except_messages state1 state2 =
  all_true @@ List.remove_assoc "messages" @@ full_eq state1 state2

let () = test_file "Unit"

module Node_actions = struct
  let group = "Node actions"

  let desc =
    "Application of each node action from a state in which it is enabled"

  (** if [node] is inactive on [chain], then [node] can activate on [chain] *)
  let activate_test () =
    let initial = init 2 in
    let state = initial in
    let node = Id.id 1 in
    let chain = Chain.id 1 in
    let final =
      activate ~trace:false state node chain ;
      state
    in
    if List.mem chain @@ active initial node then initial = final
    else
      let init_active = active initial node in
      active final node = List.sort Chain.compare @@ (chain :: init_active)
      && equal_except_active initial final

  let deactivate_test () =
    let initial = init 2 in
    let node = Id.id 1 in
    let chain = Chain.id 1 in
    let initial =
      activate ~trace:false initial node chain ;
      initial
    in
    let state = initial in
    let final =
      deactivate ~trace:false state node chain ;
      state
    in
    if List.mem chain @@ inactive_chains initial node then initial = final
    else
      let init_active = active initial node in
      ( active final node
      = List.(sort Chain.compare @@ remove_one chain init_active) )
      && equal_except_active initial final

  let node_receive_test () =
    let initial = init 2 in
    let node1 = Id.id 1 in
    let node2 = Id.id 2 in
    let chain = Chain.id 1 in
    let msg =
      Message.Msg (node2, node1, Msg.(Req (Get_current_branch chain)))
    in
    let initial =
      activate ~trace:false initial node1 chain ;
      activate ~trace:false initial node2 chain ;
      send_msg initial node1 chain msg ;
      initial
    in
    let final = initial in
    let final =
      receive_msg ~trace:false final node1 chain msg ;
      final
    in
    if
      not
        List.(
          for_all (fun n -> mem n @@ active_nodes initial chain) [node1; node2])
    then initial = final
    else
      let msgs = messages_list initial node1 chain in
      (msgs = List.(sort Message.compare @@ messages_list final node1 chain))
      && equal_except_messages initial final

  let tests =
    [ ("activation should change at most one node's active list", activate_test);
      ( "deactivation should change at most one node's active list",
        deactivate_test );
      ( "nodes receiving messages should only change their messages",
        node_receive_test ) ]
end

let () = run_tests (module Node_actions)

module System_actions = struct
  let group = "System actions"

  let desc =
    "Application of each system action from a state in which it is enabled"

  let tests = [("TODO write system tests", fun () -> false)]
end

let () = run_tests (module System_actions)

let () = print_all "unit"
