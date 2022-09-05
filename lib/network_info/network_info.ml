(** State of the network *)

open Basic
include Type
include View
open Printf

(** {1 Enabling condition checks} *)

(** verifies that [chain] is valid in the current context *)
let check_chains info chain exp =
  if chain <= Chain.id 0 || chain > info.chain then
    printf "chain %s is not in the set of chains on this network: %s\n"
      Chain.(view chain)
    @@ view_chains info
  else exp ()

(** verifies that [node] is active on [chain] in the current context *)
let check_active info chain node exp =
  check_chains info chain (fun () ->
      if not (List.mem node @@ active info chain) then
        printf "node %s is not active on chain %s\n"
          Id.(view node)
          Chain.(view chain)
      else exp ())

(** verifies that there is an active node on [chain] *)
let check_any_active info chain exp =
  check_chains info chain (fun () ->
      if active info chain = [] then
        printf "There are no active nodes on chain %s\n" Chain.(view chain)
      else exp ())

(** verifies that [branch] is valid and can be added to [chain] *)
let check_branch_add info chain branch exp =
  check_chains info chain (fun () ->
      let open Branch in
      if to_int branch < 0 then
        printf "%s is an invalid branch\n" @@ view branch
      else if branch <= current_branch info chain then
        printf "branch %s already exists on chain %s\n" (view branch)
          Chain.(view chain)
      else exp ())

(** verifies that that [branch] already exists on [chain] *)
let check_branch_exists info chain branch exp =
  check_chains info chain (fun () ->
      let open Branch in
      if to_int branch < 0 then
        printf "%s is an invalid branch\n" @@ view branch
      else if branch > current_branch info chain then
        printf "branch %s does not exist on chain %s\n" (view branch)
          Chain.(view chain)
      else exp ())

let check_height_add info chain branch h exp =
  check_branch_exists info chain branch (fun () ->
      let hgt = current_height info chain branch in
      if h < 0 then printf "%d is an invalid height\n" h
      else if h <= hgt then
        printf
          "branch %s on chain %s is already at height %d which is higher than %d\n"
          Branch.(view branch)
          Chain.(view chain)
          hgt h
      else exp ())

let check_sent info chain exp =
  if sent info chain sys = Messages.empty then
    printf "the system has no messages waiting on chain %s\n" Chain.(view chain)
  else exp ()

let check_sysmsgs info chain exp =
  if Queue.is_empty @@ sysmsgs info chain then
    printf "the system has no messages to handle on chain %s\n"
      Chain.(view chain)
  else exp ()

(** [sys] receives a [msg] on [chain] *)
let receive_sys info chain msg =
  let sys_msgs = sysmsgs info chain in
  let sys_msgs =
    Queue.push msg sys_msgs;
    sys_msgs
  in
  let sys_sent = Messages.remove msg @@ sent_sys info chain in
  info.sysmsgs <- CMap.add chain sys_msgs info.sysmsgs;
  info.sent <- CNMap.add (chain, sys) sys_sent info.sent

(** {1 System actions} *)

(** {2 New_block} *)

(* verify that it is appropriate to add [block] to the current context *)
let new_block ?(trace = true) info (block : Block.t) =
  let chain = block.header.chain in
  let branch = block.header.branch in
  let height = block.header.height in
  let exp_hgt = current_height info chain branch + 1 in
  check_height_add info chain branch height (fun () ->
      if height <> exp_hgt then
        printf "The next block on chain %s branch %s must at height %d\n"
          Chain.(view chain)
          Branch.(view branch)
          exp_hgt
      else
        let blocks = Blocks.insert block (blocks info chain branch) in
        (* add action to execution trace *)
        if trace then Execution.sys_new_block chain block info.trace;
        (* add [block] to collection on [branch] of [chain] *)
        info.blocks <- CBMap.add (chain, branch) blocks info.blocks)

let new_block' ?(trace = true) info c b height n =
  let block : Block.t =
    { header = { chain = Chain.id c; branch = Branch.id b; height }
    ; ops = (height, n)
    }
  in
  new_block ~trace info block

(** randomize over chains, branches, num_ops, (height is determined) *)
let new_block_random ?(trace = true) info =
  let c = Random.int Chain.(to_int info.chain) + 1 in
  let chain = Chain.id c in
  let b = Random.int Branch.(to_int (current_branch info chain) + 1) in
  let h = current_height info chain (Branch.id b) + 1 in
  let n = Random.int 10 in
  new_block' ~trace info c b h n

let new_block_rand_ops ?(trace = true) info c b h =
  let num_ops = Random.int 10 in
  new_block' ~trace info c b h num_ops

(** {2 New_chain} *)
let new_chain ?(trace = true) info =
  let c = Chain.to_int info.chain + 1 in
  let chain = Chain.(id c) in
  (* add action to execution trace *)
  if trace then Execution.sys_new_chain chain info.trace;
  (* update chain *)
  info.chain <- chain;
  (* new chain gets branch 0 *)
  info.branch <- CMap.add chain Branch.(id 0) info.branch;
  (* and block 0 *)
  new_block_rand_ops ~trace info c 0 0

(** {2 New_branch} *)

(* Add new branch on existing [chain] *)
let new_branch ?(trace = true) info chain =
  check_chains info chain (fun () ->
      let b = (Branch.to_int @@ current_branch info chain) + 1 in
      let branch = Branch.id b in
      (* add action to execution trace *)
      Execution.sys_new_branch chain branch info.trace;
      (* update branch *)
      info.branch <- CMap.add chain branch info.branch;
      (* add block 0 *)
      new_block_rand_ops ~trace info Chain.(to_int chain) b 0)

(** {2 Receive} *)

(* [sys] rececieves the "first" message from their mailbox *)
let receive_first ?(trace = true) info chain =
  check_sent info chain (fun () ->
      match Messages.to_list @@ sent_sys info chain with
      | [] -> assert false
      | msg :: _ ->
        (* add action to execution trace *)
        if trace then Execution.sys_recv chain msg info.trace;
        (* [sys] receives [msg] *)
        receive_sys info chain msg)

(** {2 Advertise} *)

let send_msg info chain node sys_msg =
  let updated = Messages.add sys_msg @@ sent info chain node in
  info.sent <- CNMap.add (chain, node) updated info.sent

let broadcast info chain gen_msg =
  let open List in
  let active_msgs = map (fun n -> (n, gen_msg n)) @@ active info chain in
  iter (fun (n, m) -> send_msg info chain n m) active_msgs

(** {3 Current_branch} *)

let advertise_branch_one ?(trace = true) info chain node =
  check_active info chain node (fun () ->
      let branch = current_branch info chain in
      let msg =
        Message.Msg (Id.id 0, node, Msg.(Adv (Current_branch (chain, branch))))
      in
      (* add action to execution trace *)
      if trace then Execution.sys_adv_one node chain msg info.trace;
      (* [sys] sends [msg] to [node] *)
      send_msg info chain node msg)

let advertise_branch_all ?(trace = true) info chain =
  check_any_active info chain (fun () ->
      let branch = current_branch info chain in
      let msg = Msg.(Adv (Current_branch (chain, branch))) in
      let gen_msg n = Message.Msg (Id.id 0, n, msg) in
      (* add action to execution trace *)
      if trace then Execution.sys_adv_all chain msg info.trace;
      (* [sys] sends [msg] to [node] *)
      broadcast info chain gen_msg)

(** {3 Current_head} *)
let advertise_head_one ?(trace = true) info chain node branch =
  check_active info chain node (fun () ->
      let height = current_height info chain branch in
      let msg =
        Message.Msg
          (Id.id 0, node, Msg.(Adv (Current_head (chain, branch, height))))
      in
      (* add action to execution trace *)
      if trace then Execution.sys_adv_one node chain msg info.trace;
      (* [sys] sends [msg] to [node] *)
      send_msg info chain node msg)

let advertise_head_all ?(trace = true) info chain branch =
  check_any_active info chain (fun () ->
      let height = current_height info chain branch in
      let msg = Msg.(Adv (Current_head (chain, branch, height))) in
      let gen_msg n = Message.Msg (Id.id 0, n, msg) in
      (* add action to execution trace *)
      if trace then Execution.sys_adv_all chain msg info.trace;
      (* [sys] sends [msg] to [node] *)
      broadcast info chain gen_msg)

(** {2 Node actions that affect the network} *)

let activate_node info chain node =
  let updated = List.sort_uniq compare @@ (node :: active info chain) in
  info.active <- CMap.add chain updated info.active

let deactivate_node info chain node =
  let updated = List.remove_all node @@ active info chain in
  info.active <- CMap.add chain updated info.active

let receive_msg info chain node msg =
  let updated = Messages.remove msg @@ sent info chain node in
  info.sent <- CNMap.add (chain, node) updated info.sent
