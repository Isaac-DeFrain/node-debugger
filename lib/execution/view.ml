open! Action
open! Basic
open Printf

let view_nodes ns = sprintf "[%s]" (String.concat ", " @@ List.map Id.view ns)

let rec view = function
  | Node action -> sprintf "Node(%s)" @@ view_node action
  | Sys action -> sprintf "Sys(%s)" @@ view_sys action

and view_node = function
  | Activate (n, c) ->
    sprintf "Activate(node %s, chain %s)" Id.(view n) Chain.(view c)
  | Deactivate (n, c) ->
    sprintf "Deactivate(node %s, chain %s)" Id.(view n) Chain.(view c)
  | Recv_node (n, _, m) ->
    sprintf "Receive(from %s, %s)" Id.(view n) Message.(view m)
  | Ack (n, _, m) -> sprintf "Ack(node %s, %s)" Id.(view n) Message.(view m)
  | Adv_one (s, r, _, m) ->
    sprintf "Adv_one(from %s, to %s, %s)"
      Id.(view s)
      Id.(view r)
      Message.(view m)
  | Adv_all (n, c, m, a) ->
    sprintf "Adv_all(from %s, chain %s, %s, active: %s)"
      Id.(view n)
      Chain.(view c)
      Msg.(view m)
      (view_nodes a)
  | Err (n, _, m) -> sprintf "Err(from %s, %s)" Id.(view n) Msg.(view m)
  | Req_one (s, r, _, m) ->
    sprintf "Req_one(from %s, to %s, %s)"
      Id.(view s)
      Id.(view r)
      Message.(view m)
  | Req_all (n, c, m) ->
    sprintf "Req_all(from %s, chain %s, %s)"
      Id.(view n)
      Chain.(view c)
      Msg.(view m)
  | Update_branch (n, c, b) ->
    sprintf "Update_branch(node %s, chain %s, branch %s)"
      Id.(view n)
      Chain.(view c)
      Branch.(view b)
  | Update_height (n, c, b, h) ->
    sprintf "Update_height(node %s, chain %s, branch %s, height %d)"
      Id.(view n)
      Chain.(view c)
      Branch.(view b)
      h
  | Update_headers (n, _, hdr) ->
    sprintf "Update_headers(node %s, header %s)" Id.(view n) Header.(view hdr)
  | Update_blocks (n, _, blk) ->
    sprintf "Update_blocks(node %s, block %s)" Id.(view n) Block.(view blk)
  | Handle_ack (n, c, ack) ->
    sprintf "Handle_ack(node %s, chain %s, %s)"
      Id.(view n)
      Chain.(view c)
      Msg.(view_ack ack)
  | Handle_adv (n, c, adv) ->
    sprintf "Handle_adv(node %s, chain %s, %s)"
      Id.(view n)
      Chain.(view c)
      Msg.(view_adv adv)
  | Handle_err (n, c, err) ->
    sprintf "Handle_err(node %s, chain %s, %s)"
      Id.(view n)
      Chain.(view c)
      Msg.(view_err err)
  | Handle_req (n, c, req) ->
    sprintf "Handle_req(node %s, chain %s, %s)"
      Id.(view n)
      Chain.(view c)
      Msg.(view_req req)

and view_sys = function
  | New_block (_, b) -> sprintf "New_block %s" Block.(view b)
  | New_branch (c, b) ->
    sprintf "New_branch(chain %s, branch %s)" Chain.(view c) Branch.(view b)
  | New_chain c -> sprintf "New_chain %s" Chain.(view c)
  | Recv_sys (_, m) -> sprintf "Receive(%s)" Message.(view m)
  | Adv_one_sys (n, _, m) ->
    sprintf "Adv_one(to %s, %s)" Id.(view n) Message.(view m)
  | Adv_all_sys (c, m) ->
    sprintf "Adv_all(chain %s, %s)" Chain.(view c) Msg.(view m)

let view_many acts = String.concat_comma @@ List.map view acts
