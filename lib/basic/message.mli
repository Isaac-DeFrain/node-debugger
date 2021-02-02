(** Msg(from, to, contents) *)

(** type of a message: sender, receiver, contents *)
type t = Msg of Id.t * Id.t * Msg.t

val compare : t -> t -> int

(** string representation of a message *)
val view : t -> string
