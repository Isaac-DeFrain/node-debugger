(** Trace of the state during execution *)

open Basic
module Action = Action
open View

(** An execution [trace] is a list of actions along with the number of steps
    taken *)
type t =
  { mutable actions : Action.t list
  ; mutable steps : int
  }

let init () = { actions = []; steps = 0 }

(** Adds an [action] and increments the [step] *)
let add action t =
  let acts = action :: t.actions in
  t.actions <- acts;
  t.steps <- t.steps + 1

exception Empty

exception Step_action_mismatch of string * int

exception Step_must_be_positive

let check_valid t exp =
  if t.steps < 0 then raise Step_must_be_positive
  else
    let len = List.length t.actions in
    if t.steps <> len then
      raise @@ Step_action_mismatch (view_many t.actions, t.steps)
    else exp ()

(** string representation of an execution [trace] *)
let view t =
  check_valid t (fun () ->
      let len = List.length t.actions in
      let actions = t.actions in
      let rec aux acc stp = function
        | [] -> acc
        | hd :: tl ->
          aux (Printf.sprintf "Step %d: %s" stp (view hd) :: acc) (stp - 1) tl
      in
      aux [] len actions |> String.concat_endline)

(** Removes the last [action] and decrements the [step] *)
let prev t =
  let len = List.length t.actions in
  if t.steps <> len then
    raise @@ Step_action_mismatch (view_many t.actions, t.steps)
  else if t.steps = 0 then raise Empty
  else
    match t.actions with
    | _ :: tl ->
      t.actions <- tl;
      t.steps <- t.steps - 1
    | _ -> assert false

let join t1 t2 =
  match (t1, t2) with
  | [], _ | _, [] -> t1 @ t2
  | _ ->
    let open List in
    let hd2 = hd_exn t2 in
    let lst = rev t1 |> hd_exn in
    if lst = hd2 then t1 @ tl_exn t2
    else (
      Printf.printf
        "The last action in the first argument:\n\
        \  %s\n\
         does not match the first action in the second argument:\n\
        \  %s" (view lst) (view hd2);
      [])

(* System actions *)
open Action

let sys_new_block chain block = add (Sys (New_block (chain, block)))

let sys_new_branch chain branch = add (Sys (New_branch (chain, branch)))

let sys_new_chain chain = add (Sys (New_chain chain))

let sys_recv chain msg = add (Sys (Recv_sys (chain, msg)))

let sys_adv_one node chain msg = add (Sys (Adv_one_sys (node, chain, msg)))

let sys_adv_all chain msg = add (Sys (Adv_all_sys (chain, msg)))

(* Node actions *)
let node_activate node chain = add (Node (Activate (node, chain)))

let node_deactivate node chain = add (Node (Deactivate (node, chain)))

let node_recv node chain msg = add (Node (Recv_node (node, chain, msg)))

let node_ack node chain msg = add (Node (Ack (node, chain, msg)))

(** node_adv_one [sndr] [rcvr] [msg] *)
let node_adv_one sndr rcvr chain msg =
  add (Node (Adv_one (sndr, rcvr, chain, msg)))

let node_adv_all node chain msg actives =
  add (Node (Adv_all (node, chain, msg, actives)))

let node_err node chain msg = add (Node (Err (node, chain, msg)))

(** node_req_one [sndr] [rcvr] [msg] *)
let node_req_one sndr rcvr chain msg =
  add (Node (Req_one (sndr, rcvr, chain, msg)))

let node_req_all node chain msg = add (Node (Req_all (node, chain, msg)))

let node_update_branch node chain branch =
  add (Node (Update_branch (node, chain, branch)))

let node_update_height node chain branch h =
  add (Node (Update_height (node, chain, branch, h)))

let node_update_headers node chain hdr =
  add (Node (Update_headers (node, chain, hdr)))

let node_update_blocks node chain block =
  add (Node (Update_blocks (node, chain, block)))

let node_handle_ack node chain ack = add (Node (Handle_ack (node, chain, ack)))

let node_handle_adv node chain adv = add (Node (Handle_ack (node, chain, adv)))

let node_handle_err node chain err = add (Node (Handle_ack (node, chain, err)))

let node_handle_req node chain req = add (Node (Handle_ack (node, chain, req)))
