[@@@warning "-27"]

(** {1 Node & System Actions} *)

open Basic
include Type
include View
open Printf
module Enabling_condition = Enabling_condition

(** {2 Enabling condition checks} *)

let not_active node chain =
  printf "node %s is not active on chain %s\n" Id.(view node) Chain.(view chain)

let already_active node chain =
  printf "node %s is already active on chain %s\n"
    Id.(view node)
    Chain.(view chain)

(** check if [node] is a valid node *)
let check_nodes info node exp =
  if not (Array.mem node info.nodes) then
    printf "node %s is not in the set of nodes: %s\n" Id.(view node)
    @@ view_nodes info
  else exp ()

(** verifies that [node] and [chain] are valid and [node] is active on [chain] *)
let check_active info node chain exp =
  check_nodes info node (fun () ->
      Network_info.check_chains info.network chain (fun () ->
          if not (List.mem (active info node) chain ~equal:Chain.equal) then
            not_active node chain
          else exp ()))

(** check if [node] knows about [branch] on [chain] *)
let check_branches info node chain branch exp =
  check_active info node chain (fun () ->
      if not (List.mem (branches info node chain) branch ~equal:Branch.equal)
      then
        printf "node %s does not know about branch %s on chain %s"
          Id.(view node)
          Branch.(view branch)
          Chain.(view chain)
      else exp ())

(** check if there are other active nodes on [chain] besides [node] *)
let check_other_active info node chain exp =
  check_active info node chain (fun () ->
      (* remove [node] and [sys] *)
      let other_active =
        List.(remove_all node @@ active_nodes info chain |> remove_all sys)
      in
      if other_active = [] then
        printf "node %s is the only active node on chain %s"
          Id.(view node)
          Chain.(view chain)
      else exp ())

(** check if [node] has a message to handle on [chain] *)
let check_messages info node chain exp =
  check_active info node chain (fun () ->
      if Queue.is_empty @@ messages info node chain then
        printf "node %s has no messages on chain %s\n"
          Id.(view node)
          Chain.(view chain)
      else exp ())

(** check if [node] has any messages waiting on [chain] *)
let check_sent info node chain exp =
  check_active info node chain (fun () ->
      let open Network_info in
      match sent_list info.network chain node with
      | [] ->
        printf "node %s has no messages waiting on chain %s\n"
          Id.(view node)
          Chain.(view chain)
      | _ -> exp ())

(** check if [node] knows about a block header on [branch] of [chain] at
    [height] *)
let check_all_headers info node chain branch height exp =
  check_active info node chain (fun () ->
      let open List in
      let blk_hdrs =
        map
          ~f:(fun (b : Block.t) -> b.header)
          (blocks_list info node chain branch)
      in
      match headers_list info node chain @ blk_hdrs with
      | [] ->
        printf "node %s has no headers on chain %s\n"
          Id.(view node)
          Chain.(view chain)
      | hs -> (
        match
          filter
            ~f:(fun (h : Header.t) -> h.branch = branch && h.height = height)
            hs
        with
        | [] ->
          printf
            "node %s doe not have a header on branch %s of chain %s at height %d\n"
            Id.(view node)
            Branch.(view branch)
            Chain.(view chain)
            height
        | hdrs -> exp hdrs))

(** check if [node] has a block at [height] on [branch] of [chain] *)
let check_for_block_at_height info node chain branch height exp =
  check_branches info node chain branch (fun () ->
      if not (has_block_at_height info node chain branch height) then
        printf
          "node %s does not have a block at height %d on branch %s of chain %s"
          Id.(view node)
          height
          Branch.(view branch)
          Chain.(view chain)
      else exp ())

(** {2 Node actions} *)

(** a [msg] is sent to [rcvr] on [chain] *)
let send_msg info rcvr chain msg =
  let open Network_info in
  let updated = Messages.add msg @@ sent info.network chain rcvr in
  info.network.sent <- CNMap.add (chain, rcvr) updated info.network.sent

let add_msg info node chain msg =
  let messages = messages info node chain in
  let updated =
    Queue.enqueue messages msg;
    messages
  in
  info.messages <- NCMap.add (node, chain) updated info.messages

(** [node] broadcasts messages to all other active nodes on [chain], not [sys] *)
let broadcast info node chain gen_msg =
  let open List in
  let other_active = remove_all node @@ active_nodes info chain in
  iter ~f:(fun n -> send_msg info n chain @@ gen_msg n) other_active

(** [node] broadcasts messages to all other active nodes on [chain] and [sys] *)
let broadcast_sys info node chain gen_msg =
  let open List in
  let other_active_sys = sys :: (remove_all node @@ active_nodes info chain) in
  iter ~f:(fun r -> send_msg info r chain @@ gen_msg r) other_active_sys

(** [node] registers an [exp] on [chain] *)
let register_exp info node chain exp =
  let expected = Messages.add exp @@ expect info node chain in
  info.expect <- NCMap.add (node, chain) expected info.expect

(** [node] removes an [exp] on [chain] *)
let remove_exp info node chain exp =
  let expected = Messages.remove exp @@ expect info node chain in
  info.expect <- NCMap.add (node, chain) expected info.expect

(** {3 Activate/Deactivate} *)

(** [node] becomes active on [chain] *)
let activate ?(trace = true) info node chain =
  check_nodes info node (fun () ->
      Network_info.check_chains info.network chain (fun () ->
          if List.mem (active info node) chain ~equal:Chain.equal then
            printf "node %s cannot activate on chain %s\n"
              Id.(view node)
              Chain.(view chain)
          else if (* add action to execution trace *)
                  trace then
            Execution.node_activate node chain info.network.trace;
          (* add [node] to active on [chain] *)
          Network_info.activate_node info.network chain node;
          (* add [node] to active on [chain] *)
          let open List in
          match IdMap.find_opt node info.active with
          | None -> info.active <- IdMap.add node [ chain ] info.active
          | Some chains ->
            if mem chains chain ~equal:Chain.equal then
              already_active node chain
            else
              let updated =
                dedup_and_sort ~compare:Chain.compare (chain :: chains)
              in
              info.active <- IdMap.add node updated info.active))

let activate' info n c = activate info Id.(id n) Chain.(id c)

(** [node] becomes inactive on [chain] *)
let deactivate ?(trace = true) info node chain =
  check_active info node chain (fun () ->
      let open List in
      match IdMap.find_opt node info.active with
      | None -> not_active node chain
      | Some chains ->
        if not (mem chains chain ~equal:Chain.equal) then not_active node chain
        else
          let updated = remove_all chain chains in
          (* add action to execution trace *)
          if trace then Execution.node_deactivate node chain info.network.trace;
          (* [node] removed from active on [chain] *)
          info.active <- IdMap.add node updated info.active;
          (* add [node] to active on [chain] *)
          Network_info.deactivate_node info.network chain node)

let deactivate' info n c = deactivate info Id.(id n) Chain.(id c)

(** {3 Receiving messages} *)

(* TODO specify type of message/sender *)
let receive_first ?(trace = true) info node chain =
  check_sent info node chain (fun () ->
      let open Network_info in
      let msg = List.hd_exn @@ sent_list info.network chain node in
      (* add action to execution trace *)
      if trace then Execution.node_recv node chain msg info.network.trace;
      (* remove [msg] from [node]'s waiting messages *)
      receive_msg info.network chain node msg;
      (* add [msg] to [node]'s [messages] *)
      add_msg info node chain msg)

let receive_first' info n c = receive_first info Id.(id n) Chain.(id c)

(* check that [node] has messages to receive on [chain] *)

(** [node] receives a random message on [chain] *)
let receive_random ?(trace = true) info node chain =
  check_sent info node chain (fun () ->
      let open Network_info in
      let msg = List.random_elem @@ sent_list info.network chain node in
      (* add action to execution trace *)
      if trace then Execution.node_recv node chain msg info.network.trace;
      (* remove [msg] from [node]'s waiting messages *)
      receive_msg info.network chain node msg;
      (* add [msg] to [node]'s [messages] *)
      add_msg info node chain msg)

let receive_random' info n c = receive_random info Id.(id n) Chain.(id c)

(* check that [msg] is in [node]'s messages to receive on [chain] *)

(** [node] receives a specific [msg] on [chain] *)
let receive_msg ?(trace = true) info node chain msg =
  check_sent info node chain (fun () ->
      let open Network_info in
      match sent_list info.network chain node with
      | [] ->
        printf "node %s has not been sent any messages chain %s\n"
          Id.(view node)
          Chain.(view chain)
      | msgs ->
        if not (List.mem msgs msg ~equal:Message.equal) then
          printf
            "The message\n\
            \  %s\n\
             was not found in node %s's sent messages on chain %s:\n\
            \  %s\n"
            Message.(view msg)
            Id.(view node)
            Chain.(view chain)
            Messages.(view @@ of_list msgs)
        else if trace then
          (* add action to execution trace *)
          Execution.node_recv node chain msg info.network.trace;
        (* remove [msg] from [node]'s waiting messages *)
        Network_info.receive_msg info.network chain node msg;
        (* add [msg] to [node]'s messages *)
        add_msg info node chain msg)

let receive_msg' info n c = receive_msg info Id.(id n) Chain.(id c)

(** {3 Updates} *)

(** [node] updates branches on [chain] *)
let update_branch ?(trace = true) info node chain branch =
  check_active info node chain (fun () ->
      let bs = branches info node chain in
      if List.mem bs branch ~equal:Branch.equal then
        printf "node %s already knows about branch %s on chain %s\n"
          Id.(view node)
          Branch.(view branch)
          Chain.(view chain)
      else
        let updated =
          List.dedup_and_sort ~compare:Branch.compare (branch :: bs)
        in
        (* add action to execution trace *)
        if trace then
          Execution.node_update_branch node chain branch info.network.trace;
        (* update branches *)
        info.branches <- NCMap.add (node, chain) updated info.branches)

(** [node] updates height on [branch] of [chain] *)
let update_height ?(trace = true) info node chain branch h =
  check_active info node chain (fun () ->
      if h <= height info node chain branch then
        printf
          "node %s already knows about a block at height %d on branch %s of \
           chain %s\n"
          Id.(view node)
          h
          Branch.(view branch)
          Chain.(view chain)
      else if (* add action to execution trace *)
              trace then
        Execution.node_update_height node chain branch h info.network.trace;
      (* update height *)
      info.height <- NCBMap.add (node, chain, branch) h info.height)

(** [node] has received operations for a block header in their collection and
    adds the [block] *)
let update_blocks ?(trace = true) info node (block : Block.t) =
  let chain = block.header.chain in
  let branch = block.header.branch in
  let blocks = Blocks.insert block @@ blocks info node chain branch in
  (* add action to execution trace *)
  if trace then Execution.node_update_blocks node chain block info.network.trace;
  (* add new [block] *)
  info.blocks <- NCBMap.add (node, chain, branch) blocks info.blocks

(** {3 Sending messages} *)

open Msg

(** {4 Advertise} *)

(** [sndr] advertises current branch to [rcvr] on [chain] *)
let advertise_branch_one ?(trace = true) info sndr rcvr chain =
  check_active info sndr chain (fun () ->
      check_active info rcvr chain (fun () ->
          let open Message in
          let branch = current_branch info sndr chain in
          let adv' = Adv (Current_branch (chain, branch)) in
          let adv = Msg (sndr, rcvr, adv') in
          let exp = Msg (rcvr, sndr, Exp (expect_msg rcvr adv')) in
          (* add action to execution trace *)
          if trace then
            Execution.node_adv_one sndr rcvr chain adv info.network.trace;
          (* [adv] sent to [rcvr] *)
          send_msg info rcvr chain adv;
          (* register expectation *)
          register_exp info sndr chain exp))

let advertise_branch_one' info s r c =
  advertise_branch_one info Id.(id s) Id.(id r) Chain.(id c)

(* check that there are other active nodes on [chain] *)

(** [node] sends them their current [branch] *)
let advertise_branch_all ?(trace = true) info node chain =
  check_other_active info node chain (fun () ->
      let open Message in
      let open List in
      let branch = current_branch info node chain in
      let other_active =
        remove_all node (active_nodes info chain) |> remove_all sys
      in
      let adv' = Adv (Current_branch (chain, branch)) in
      let gen_adv rcvr = Msg (node, rcvr, adv') in
      (* add action to execution trace *)
      if trace then
        Execution.node_adv_all node chain adv' other_active info.network.trace;
      (* send each other active node the advertisement *)
      broadcast info node chain gen_adv)

let advertise_branch_all' info n c =
  advertise_branch_all info Id.(id n) Chain.(id c)

(* [sndr] and [rcvr] must both be active on [chain] *)

(** [sndr] advertises their current head of [branch] on [chain] to [rcvr] *)
let advertise_head_one ?(trace = true) info sndr rcvr chain branch =
  check_active info sndr chain (fun () ->
      (* TODO check_branches info node chain branch (fun () -> ...) *)
      check_active info rcvr chain (fun () ->
          let open Message in
          let height = height info sndr chain branch in
          let adv' = Adv (Current_head (chain, branch, height)) in
          let adv = Msg (sndr, rcvr, adv') in
          let exp = Msg (rcvr, sndr, Exp (expect_msg rcvr adv')) in
          (* add action to execution trace *)
          if trace then
            Execution.node_adv_one sndr rcvr chain adv info.network.trace;
          (* [adv] sent to [rcvr] *)
          send_msg info rcvr chain adv;
          (* register expectation *)
          register_exp info sndr chain exp))

let advertise_head_one' info s r c b =
  advertise_head_one info Id.(id s) Id.(id r) Chain.(id c) Branch.(id b)

(** [node] advertises their current head to all other active nodes on [chain] *)
let advertise_head_all ?(trace = true) info node chain branch =
  check_other_active info node chain (fun () ->
      check_branches info node chain branch (fun () ->
          let open Message in
          let open List in
          (* let branch = current_branch info node chain in *)
          let other_active =
            remove_all node (active_nodes info chain) |> remove_all sys
          in
          let adv' = Adv (Current_branch (chain, branch)) in
          let gen_adv rcvr = Msg (node, rcvr, adv') in
          (* add action to execution trace *)
          if trace then
            Execution.node_adv_all node chain adv' other_active
              info.network.trace;
          (* send each other active node the advertisement *)
          iter
            ~f:(fun rcvr -> send_msg info rcvr chain (gen_adv rcvr))
            other_active))

(** {4 Request} *)

(* TODO requests *)

(* making requests *)
let request_msg ?(trace = true) info sndr chain ?rcvr req =
  check_active info sndr chain (fun () ->
      match rcvr with
      | None ->
        (* add action to execution trace *)
        if trace then
          Execution.node_req_all sndr chain (Req req) info.network.trace;
        (* request is sent to all active nodes on [chain] - [sndr] + [sys] *)
        broadcast_sys info sndr chain @@ fun rcvr -> Msg (sndr, rcvr, Req req)
      | Some rcvr ->
        check_active info rcvr chain (fun () ->
            let msg = Message.Msg (sndr, rcvr, Req req) in
            (* add action to execution trace *)
            if trace then
              Execution.node_req_one sndr rcvr chain msg info.network.trace;
            (* request is sent to the active node [rcvr] on [chain] *)
            send_msg info rcvr chain msg))

(** {4 Acknowledge} *)

(* TODO acknowledgments *)

(** {4 Error}*)

(* TODO error *)

(** {3 Handling messages} *)

(* TODO trace in handle instead? *)
let send_ack ?(trace = true) info sndr rcvr chain ack =
  check_active info sndr chain (fun () ->
      check_active info rcvr chain (fun () ->
          let ack = Message.Msg (sndr, rcvr, Ack ack) in
          (* add action to execution trace *)
          if trace then Execution.node_ack sndr chain ack info.network.trace;
          (* add ack to network_info.sent *)
          send_msg info rcvr chain ack))

(* TODO trace here or in handle? *)
let send_adv_one ?(trace = true) info sndr rcvr chain adv =
  check_active info sndr chain (fun () ->
      check_active info rcvr chain (fun () ->
          let adv = Message.Msg (sndr, rcvr, Adv adv) in
          (* add action to execution trace *)
          if trace then
            Execution.node_adv_one sndr rcvr chain adv info.network.trace;
          (* add ack to network_info.sent *)
          send_msg info rcvr chain adv))

(* TODO trace here or in handle? *)
let send_adv_all info node chain branch adv =
  check_active info node chain (fun () -> printf "TODO - send_adv_all")

(* TODO trace here or in handle? *)
let send_req info node chain req =
  check_active info node chain (fun () -> printf "TODO - send_req")

(* TODO trace here or in handle? *)
let send_err info node chain err =
  check_active info node chain (fun () -> printf "TODO - send_err")

(* TODO only in response to Get_block_header *)
let send_block_header ?(trace = true) info sndr rcvr chain branch height =
  check_active info rcvr chain (fun () ->
      check_all_headers info sndr chain branch height (fun hdrs ->
          let open Message in
          let adv' = Adv (Current_head (chain, branch, height)) in
          let adv = Msg (sndr, rcvr, adv') in
          let exp = Msg (rcvr, sndr, Exp (expect_msg rcvr adv')) in
          (* add action to execution trace *)
          if trace then
            Execution.node_adv_one sndr rcvr chain adv info.network.trace;
          (* [adv] sent to [rcvr] *)
          send_msg info rcvr chain adv;
          (* register expectation *)
          register_exp info sndr chain exp))

(* TODO only in response to Get_block_header *)
let advertise_ops ?(trace = true) info sndr rcvr chain branch height =
  check_for_block_at_height info sndr chain branch height (fun () ->
      check_active info rcvr chain (fun () ->
          let open Message in
          match block_at_height_on info sndr chain branch height with
          | Some ({ header; ops } : Block.t) ->
            let msg =
              Msg (sndr, rcvr, Adv (Operations (chain, branch, height, ops)))
            in
            (* add action to execution trace *)
            if trace then
              Execution.node_adv_one sndr rcvr chain msg info.network.trace;
            (* [sndr] sends [rcvr] ops *)
            send_msg info rcvr chain msg
          | None -> assert false))

(** [node] handles a message on [chain] *)
let rec handle info node chain =
  check_messages info node chain
    (let open Message in
    let open Queue in
    let msgs = messages info node chain in
    let (Msg (sndr, _, msg)) = dequeue_exn msgs in
    match msg with
    | Ack ack -> handle_ack info node sndr chain ack
    | Adv adv -> handle_adv info node chain adv
    | Err err -> handle_err info node chain err
    | Req req -> handle_req info node chain req
    | Exp _ -> assert false)

(* expectation messages are not sent between nodes *)
and handle_ack ?(trace = true) info node from chain ack () =
  let open Message in
  (* add action to execution trace *)
  if trace then Execution.node_handle_ack node chain ack info.network.trace;
  let exp = Msg (from, node, Exp (expect_msg from @@ Ack ack)) in
  (* remove ack from expect set *)
  remove_exp info node chain exp

and handle_adv info node chain adv () =
  (* check if an update needs to occur and do it *)
  raise (Failure "TODO - handle_adv")

and handle_err info node chain err () =
  (* remove corresponding message from expect set *)
  raise (Failure "TODO - handle_err")

and handle_req info node chain req () =
  (* remove ack from expect set *)
  raise (Failure "TODO - handle_req")

(** {2 System actions} *)

let new_chain info = Network_info.new_chain info.network

let new_branch info = Network_info.new_branch info.network

let new_branch' info c = new_branch info Chain.(id c)

let new_block info = Network_info.new_block info.network

let new_block' info = Network_info.new_block' info.network

(* TODO handling sysmsgs *)

(** {2 Enabling conditions} *)

let enabled info =
  let sys_enabled =
    String.concat_endline2 @@ Enabling_condition.view_enabled_sys info
  in
  let node_enabled =
    String.concat_endline2 @@ Enabling_condition.view_enabled_node info
  in
  printf "Enabled system actions:\n  %s\nEnabled node actions:\n  %s"
    sys_enabled node_enabled
