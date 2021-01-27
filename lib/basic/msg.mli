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

val compare : t -> t -> int

val ack_of_adv : adv_msg -> ack_msg

exception Request_can_be_handled of req_msg

val err_of_req : req_msg -> err_msg

val view : t -> string
