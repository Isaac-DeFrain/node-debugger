type t =
  | Ack of ack_msg
  | Adv of adv_msg
  | Err of err_msg
  | Exp of exp_msg
  | Req of req_msg
[@@deriving compare, equal]

and ack_msg =
  | Ack_current_branch of Chain.t
  | Ack_current_head of Chain.t * Branch.t
  | Ack_block_header of Chain.t * Branch.t * Int.t
  | Ack_operations of Chain.t * Branch.t * Int.t
[@@deriving compare, equal]

and adv_msg =
  | Current_branch of Chain.t * Branch.t
  | Current_head of Chain.t * Branch.t * Int.t
  | Block_header of Chain.t * Branch.t * Int.t * Header.t
  | Operations of Chain.t * Branch.t * Int.t * Ops.t
[@@deriving compare, equal]

and err_msg =
  | Err_block_header of Chain.t * Branch.t * Int.t
  | Err_operations of Chain.t * Branch.t * Int.t
[@@deriving compare, equal]

and exp_msg =
  | Exp_current_branch of Id.t * Chain.t
  | Exp_ack_current_branch of Id.t * Chain.t
  | Exp_current_head of Id.t * Chain.t * Branch.t
  | Exp_ack_current_head of Id.t * Chain.t * Branch.t
  | Exp_block_header of Id.t * Chain.t * Branch.t * Int.t
  | Exp_ack_block_header of Id.t * Chain.t * Branch.t * Int.t
  | Exp_operations of Id.t * Chain.t * Branch.t * Int.t
  | Exp_ack_operations of Id.t * Chain.t * Branch.t * Int.t
[@@deriving compare, equal]

and req_msg =
  | Get_current_branch of Chain.t
  | Get_current_head of Chain.t * Branch.t
  | Get_block_header of Chain.t * Branch.t * Int.t
  | Get_operations of Chain.t * Branch.t * Int.t
[@@deriving compare, equal]

val expect_msg : Id.t -> t -> exp_msg

exception Request_can_be_handled of req_msg

val err_of_req : req_msg -> err_msg

val view_ack : ack_msg -> string

val view_adv : adv_msg -> string

val view_err : err_msg -> string

val view_exp : exp_msg -> string

val view_req : req_msg -> string

val view : t -> string
