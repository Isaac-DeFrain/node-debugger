(** Msg(from, to, contents) *)

type t = Msg of Id.t * Id.t * Msg.t

val compare : t -> t -> int

val view : t -> string
