(** Content of a message *)

type t =
  | Ack of ack_msg
  | Adv of adv_msg
  | Err of err_msg
  | Req of req_msg

and ack_msg =
  | Ack_current_branch of Chain.t
  | Ack_current_head of Chain.t * Branch.t
  | Ack_block_header of Chain.t * Branch.t * int
  | Ack_operations of Chain.t * Branch.t * int

and adv_msg =
  | Adv_current_branch of Chain.t * Branch.t
  | Adv_current_head of Chain.t * Branch.t * int
  | Adv_block_header of Header.t
  | Adv_operations of Chain.t * Branch.t * int * Ops.t

and err_msg =
  | Err_block_header of Chain.t * Branch.t * int
  | Err_operations of Chain.t * Branch.t * int

and req_msg =
  | Get_current_branch of Chain.t
  | Get_current_head of Chain.t * Branch.t
  | Get_block_header of Chain.t * Branch.t * int
  | Get_operations of Chain.t * Branch.t * int

let compare = compare

let ack_of_adv = function
| Adv_current_branch (c, _) -> Ack_current_branch c
| Adv_current_head (c, b, _) -> Ack_current_head (c, b)
| Adv_block_header hdr -> Ack_block_header (hdr.chain, hdr.branch, hdr.height)
| Adv_operations (c, b, h, _) -> Ack_operations (c, b, h)

exception Request_can_be_handled of req_msg

let err_of_req = function
| Get_block_header (c, b, h) -> Err_block_header (c, b, h)
| Get_operations (c, b, h) -> Err_operations (c, b, h)
| r -> raise (Request_can_be_handled r)

(* viewing functions *)
let rec view = function
  | Ack ack -> view_ack ack
  | Adv adv -> view_adv adv
  | Err err -> view_err err
  | Req req -> view_req req

and view_ack =
  let open Printf in  
  function
  | Ack_current_branch c ->
      sprintf "Ack_current_branch( chain %s )" (Chain.view c)
  | Ack_current_head (c, b) ->
      sprintf "Ack_current_head( chain %s, branch %s )" (Chain.view c) (Branch.view b)
  | Ack_block_header (c, b, h) ->
      sprintf "Ack_block_header( chain %s, branch %s, height %d )" (Chain.view c) (Branch.view b) h
  | Ack_operations (c, b, h) ->
      sprintf "Ack_operations( chain %s, branch %s, height %d )" (Chain.view c) (Branch.view b) h

and view_adv =
  let open Printf in
  function
  | Adv_current_branch (c, b) ->
      sprintf "Adv_current_branch( chain %s, branch %s )" (Chain.view c) (Branch.view b)
  | Adv_current_head (c, b, h) ->
      sprintf "Adv_current_head( chain %s, branch %s, height %d )" (Chain.view c) (Branch.view b) h
  | Adv_block_header hdr ->
      sprintf "Adv_block_header( %s )" (Header.view hdr)
  | Adv_operations (c, b, h, ops) ->
      sprintf "Adv_operations( chain %s, branch %s, height %d, ops %s )"
      (Chain.view c) (Branch.view b) h (Ops.view ops)

and view_err =
  let open Printf in
  function
  | Err_block_header (c, b, h) ->
      sprintf "Err_block_header( chain %s, branch %s, height %d )" (Chain.view c) (Branch.view b) h
  | Err_operations (c, b, h) ->
      sprintf "Err_operations( chain %s, branch %s, height %d )" (Chain.view c) (Branch.view b) h

and view_req =
  let open Printf in
  function
  | Get_current_branch c ->
      sprintf "Get_current_branch( chain %s )" (Chain.view c)
  | Get_current_head (c, b) ->
      sprintf "Get_current_head( chain %s, branch %s )" (Chain.view c) (Branch.view b)
  | Get_block_header (c, b, h) ->
      sprintf "Get_block_header( chain %s, branch %s, height %d )" (Chain.view c) (Branch.view b) h
  | Get_operations (c, b, h) ->
      sprintf "Get_operations( chain %s, branch %s, height %d )" (Chain.view c) (Branch.view b) h
