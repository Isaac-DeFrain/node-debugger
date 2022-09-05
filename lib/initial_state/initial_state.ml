[@@@warning "-27"]

(** {1 Fine-grained initial state creation} *)

open Basic
open Node_info

let set_active state node cs =
  let open List in
  let cs = sort_uniq Chain.compare cs in
  state.active <- IdMap.add node cs state.active

let set_active state =
  List.(iter (fun (n, cs) -> set_active state Id.(id n) @@ map Chain.id cs))

let set_branches state node chain bs =
  let branches = List.(sort_uniq Branch.compare @@ map Branch.id bs) in
  state.branches <- NCMap.add (node, chain) branches state.branches

let set_node_branches state =
  let open List in
  let update_branches state n =
    iter (fun (c, bs) -> set_branches state Id.(id n) Chain.(id c) bs)
  in
  iter (fun (n, cbs) -> update_branches state n cbs)

let set_sys_branch state chain b =
  let open Network_info in
  state.network.branch <- CMap.add chain Branch.(id b) state.network.branch

let set_sys_branch state =
  List.iter (fun (c, b) -> set_sys_branch state Chain.(id c) b)

let set_blocks state node chain branch bs =
  let blocks = Blocks.of_list bs in
  state.blocks <- NCBMap.add (node, chain, branch) blocks state.blocks

let set_node_blocks state =
  let open List in
  let update_branch_blocks state n c =
    iter (fun (b, bs) ->
        set_blocks state Id.(id n) Chain.(id c) Branch.(id b) bs)
  in
  let update_chain_blocks state n =
    iter (fun (c, bbs) -> update_branch_blocks state n c bbs)
  in
  iter (fun (n, cbbs) -> update_chain_blocks state n cbbs)

let set_sys_blocks state chain branch bs =
  let open Network_info in
  let blocks = Blocks.of_list bs in
  state.network.blocks <- CBMap.add (chain, branch) blocks state.network.blocks

let set_sys_blocks state =
  let open List in
  let update_branch_blocks state c =
    iter (fun (b, bs) -> set_sys_blocks state Chain.(id c) Branch.(id b) bs)
  in
  iter (fun (c, bbs) -> update_branch_blocks state c bbs)

let set_expect state node chain msgs =
  let expect = Messages.of_list msgs in
  state.expect <- NCMap.add (node, chain) expect state.expect

let set_expect state =
  let open List in
  let update_expect state n =
    iter (fun (c, msgs) -> set_expect state Id.(id n) Chain.(id c) msgs)
  in
  iter (fun (n, cms) -> update_expect state n cms)

let set_headers state node chain hdrs =
  let headers = Headers.of_list hdrs in
  state.headers <- NCMap.add (node, chain) headers state.headers

let set_headers state =
  let open List in
  let set_node_headers state n =
    iter (fun (c, hs) -> set_headers state Id.(id n) Chain.(id c) hs)
  in
  iter (fun (n, chs) -> set_node_headers state n chs)

let set_messages state node chain msgs =
  let msgs = Queue.of_list msgs in
  state.messages <- NCMap.add (node, chain) msgs state.messages

let set_messages state =
  let open List in
  let set_node_msgs state n = iter (fun (c, ms) -> set_messages state n c ms) in
  iter (fun (n, cms) -> set_node_msgs state n cms)

let set_node_height state node chain branch height =
  state.height <- NCBMap.add (node, chain, branch) height state.height

let set_node_height state =
  let open List in
  let set_node_height state n =
    iter (fun (c, bhs) ->
        iter
          (fun (b, h) ->
            set_node_height state Id.(id n) Chain.(id c) Branch.(id b) h)
          bhs)
  in
  iter (fun (n, cbh) -> set_node_height state n cbh)

let set_sys_height state chain branch height =
  state.network.height <-
    Network_info.CBMap.add (chain, branch) height state.network.height

let set_sys_height state =
  let open List in
  iter (fun (c, bhs) ->
      iter (fun (b, h) -> set_sys_height state Chain.(id c) Branch.(id b) h) bhs)

(* TODO sent *)

(* TODO sysmsgs *)

let initial_state ~nodes ~chains ~active ~node_branches ~sys_branches
    ~node_blocks ~sys_blocks ~expect ~headers ~messages ~node_height ~sys_height
    ~sent ~sysmsgs =
  let state = init nodes in
  state.network.chain <- Chain.id chains;
  set_node_branches state node_branches;
  set_active state active;
  set_sys_branch state sys_branches;
  set_node_blocks state node_blocks;
  set_sys_blocks state sys_blocks;
  set_expect state expect;
  set_headers state headers;
  set_messages state messages;
  set_node_height state node_height;
  set_sys_height state sys_height
(* TODO finish updating fields *)
(* if not (Valid.valid state) then ( Printf.printf "This state is
   invalid:\n%s\n" @@ view state; raise Valid.Invalid_state) else state *)
