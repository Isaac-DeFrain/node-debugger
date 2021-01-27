type t = { chain : Chain.t; branch : Branch.t; height: int }

val compare : t -> t -> int

val view : t -> string
