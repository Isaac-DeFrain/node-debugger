(** Node and system actions *)

open Basic

type t =
  | Node of node_action
  | Sys of sys_action
[@@deriving compare]

and node_action =
  | Activate of Id.t * Chain.t
  | Deactivate of Id.t * Chain.t
  | Recv_node of Id.t * Chain.t * Message.t
  | Ack of Id.t * Chain.t * Message.t
  | Adv_one of Id.t * Id.t * Chain.t * Message.t
      (** Adv_one(from, to, chain, msg) *)
  | Adv_all of Id.t * Chain.t * Msg.t * Id.t list
  | Err of Id.t * Chain.t * Msg.t
  | Req_one of Id.t * Id.t * Chain.t * Message.t
      (** Req_one(from, to, chain, msg) *)
  | Req_all of Id.t * Chain.t * Msg.t
  | Update_branch of Id.t * Chain.t * Branch.t
  | Update_height of Id.t * Chain.t * Branch.t * int
  | Update_headers of Id.t * Chain.t * Header.t
  | Update_blocks of Id.t * Chain.t * Block.t
  | Handle_ack of Id.t * Chain.t * Msg.ack_msg
  | Handle_adv of Id.t * Chain.t * Msg.adv_msg
  | Handle_err of Id.t * Chain.t * Msg.err_msg
  | Handle_req of Id.t * Chain.t * Msg.req_msg
[@@deriving compare]

(** system has id = -1 *)
and sys_action =
  | New_block of Chain.t * Block.t
  | New_branch of Chain.t * Branch.t
  | New_chain of Chain.t
  | Recv_sys of Chain.t * Message.t
  | Adv_one_sys of Id.t * Chain.t * Message.t
  | Adv_all_sys of Chain.t * Msg.t
[@@deriving compare]
