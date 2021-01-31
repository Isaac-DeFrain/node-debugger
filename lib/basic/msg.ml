(** Content of a message *)

type t =
  | Ack of ack_msg
  | Adv of adv_msg
  | Err of err_msg
  | Exp of exp_msg
  | Req of req_msg

and ack_msg =
  | Ack_current_branch of Chain.t
  | Ack_current_head of Chain.t * Branch.t
  | Ack_block_header of Chain.t * Branch.t * int
  | Ack_operations of Chain.t * Branch.t * int

and adv_msg =
  | Current_branch of Chain.t * Branch.t
  | Current_head of Chain.t * Branch.t * int
  | Block_header of Header.t
  | Operations of Chain.t * Branch.t * int * Ops.t

and err_msg =
  | Err_block_header of Chain.t * Branch.t * int
  | Err_operations of Chain.t * Branch.t * int

and exp_msg =
  | Exp_current_branch of Id.t * Chain.t
  | Exp_ack_current_branch of Id.t * Chain.t
  | Exp_current_head of Id.t * Chain.t * Branch.t
  | Exp_ack_current_head of Id.t * Chain.t * Branch.t
  | Exp_block_header of Id.t * Chain.t * Branch.t * int
  | Exp_ack_block_header of Id.t * Chain.t * Branch.t * int
  | Exp_operations of Id.t * Chain.t * Branch.t * int
  | Exp_ack_operations of Id.t * Chain.t * Branch.t * int

and req_msg =
  | Get_current_branch of Chain.t
  | Get_current_head of Chain.t * Branch.t
  | Get_block_header of Chain.t * Branch.t * int
  | Get_operations of Chain.t * Branch.t * int

let compare = compare

(* viewing functions *)
open Printf

let rec view = function
  | Ack ack ->
      view_ack ack
  | Adv adv ->
      view_adv adv
  | Err err ->
      view_err err
  | Exp exp ->
      view_exp exp
  | Req req ->
      view_req req

and view_ack = function
  | Ack_current_branch c ->
      sprintf "Ack_current_branch( chain %s )" Chain.(view c)
  | Ack_current_head (c, b) ->
      sprintf
        "Ack_current_head( chain %s, branch %s )"
        Chain.(view c)
        Branch.(view b)
  | Ack_block_header (c, b, h) ->
      sprintf
        "Ack_block_header( chain %s, branch %s, height %d )"
        Chain.(view c)
        Branch.(view b)
        h
  | Ack_operations (c, b, h) ->
      sprintf
        "Ack_operations( chain %s, branch %s, height %d )"
        Chain.(view c)
        Branch.(view b)
        h

and view_adv = function
  | Current_branch (c, b) ->
      sprintf
        "Current_branch( chain %s, branch %s )"
        Chain.(view c)
        Branch.(view b)
  | Current_head (c, b, h) ->
      sprintf
        "Current_head( chain %s, branch %s, height %d )"
        Chain.(view c)
        Branch.(view b)
        h
  | Block_header hdr ->
      sprintf "Block_header( %s )" Header.(view hdr)
  | Operations (c, b, h, ops) ->
      sprintf
        "Operations( chain %s, branch %s, height %d, ops %s )"
        Chain.(view c)
        Branch.(view b)
        h
        (Ops.view ops)

and view_err = function
  | Err_block_header (c, b, h) ->
      sprintf
        "Err_block_header( chain %s, branch %s, height %d )"
        Chain.(view c)
        Branch.(view b)
        h
  | Err_operations (c, b, h) ->
      sprintf
        "Err_operations( chain %s, branch %s, height %d )"
        Chain.(view c)
        Branch.(view b)
        h

and view_exp = function
  | Exp_current_branch (n, c) ->
      sprintf
        "Exp_current_branch( from %s, chain %s )"
        Id.(view n)
        Chain.(view c)
  | Exp_ack_current_branch (n, c) ->
      sprintf
        "Exp_ack_current_branch( from %s, chain %s )"
        Id.(view n)
        Chain.(view c)
  | Exp_current_head (n, c, b) ->
      sprintf
        "Exp_current_head( from %s, chain %s, branch %s )"
        Id.(view n)
        Chain.(view c)
        Branch.(view b)
  | Exp_ack_current_head (n, c, b) ->
      sprintf
        "Exp_ack_current_head( from %s, chain %s, branch %s )"
        Id.(view n)
        Chain.(view c)
        Branch.(view b)
  | Exp_block_header (n, c, b, h) ->
      sprintf
        "Exp_block_header( from %s, chain %s, branch %s, height %d )"
        Id.(view n)
        Chain.(view c)
        Branch.(view b)
        h
  | Exp_ack_block_header (n, c, b, h) ->
      sprintf
        "Exp_ack_block_header( from %s, chain %s, branch %s, height %d )"
        Id.(view n)
        Chain.(view c)
        Branch.(view b)
        h
  | Exp_operations (n, c, b, h) ->
      sprintf
        "Exp_operations( from %s, chain %s, branch %s, height %d )"
        Id.(view n)
        Chain.(view c)
        Branch.(view b)
        h
  | Exp_ack_operations (n, c, b, h) ->
      sprintf
        "Exp_ack_operations( from %s, chain %s, branch %s, height %d )"
        Id.(view n)
        Chain.(view c)
        Branch.(view b)
        h

and view_req = function
  | Get_current_branch c ->
      sprintf "Get_current_branch( chain %s )" Chain.(view c)
  | Get_current_head (c, b) ->
      sprintf
        "Get_current_head( chain %s, branch %s )"
        Chain.(view c)
        Branch.(view b)
  | Get_block_header (c, b, h) ->
      sprintf
        "Get_block_header( chain %s, branch %s, height %d )"
        Chain.(view c)
        Branch.(view b)
        h
  | Get_operations (c, b, h) ->
      sprintf
        "Get_operations( chain %s, branch %s, height %d )"
        Chain.(view c)
        Branch.(view b)
        h

exception No_expectation_from of string

let rec expect_msg node = function
  | Ack ack ->
      exp_of_ack node ack
  | Adv adv ->
      exp_of_adv node adv
  | Err err ->
      exp_of_err node err
  | Req req ->
      exp_of_req node req
  | m ->
      raise (No_expectation_from (view m))

and exp_of_ack node = function
  | Ack_current_branch c ->
      Exp_ack_current_branch (node, c)
  | Ack_current_head (c, b) ->
      Exp_ack_current_head (node, c, b)
  | Ack_block_header (c, b, h) ->
      Exp_ack_block_header (node, c, b, h)
  | Ack_operations (c, b, h) ->
      Exp_ack_operations (node, c, b, h)

and exp_of_adv node = function
  | Current_branch (c, _) ->
      Exp_ack_current_branch (node, c)
  | Current_head (c, b, _) ->
      Exp_ack_current_head (node, c, b)
  | Block_header hdr ->
      Exp_ack_block_header (node, hdr.chain, hdr.branch, hdr.height)
  | Operations (c, b, h, _) ->
      Exp_ack_operations (node, c, b, h)

and exp_of_err node = function
  | Err_block_header (c, b, h) ->
      Exp_block_header (node, c, b, h)
  | Err_operations (c, b, h) ->
      Exp_operations (node, c, b, h)

and exp_of_req node = function
  | Get_current_branch c ->
      Exp_current_branch (node, c)
  | Get_current_head (c, b) ->
      Exp_current_head (node, c, b)
  | Get_block_header (c, b, h) ->
      Exp_block_header (node, c, b, h)
  | Get_operations (c, b, h) ->
      Exp_operations (node, c, b, h)

exception Request_can_be_handled of req_msg

let err_of_req = function
  | Get_block_header (c, b, h) ->
      Err_block_header (c, b, h)
  | Get_operations (c, b, h) ->
      Err_operations (c, b, h)
  | r ->
      raise (Request_can_be_handled r)
