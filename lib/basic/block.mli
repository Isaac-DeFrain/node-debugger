type t = { header : Header.t; ops : Ops.t }

val block : Header.t -> Ops.t -> t

val block' : int -> int -> int -> int -> t

val compare : t -> t -> int

val view : t -> string
