type t =
  { chain : Chain.t
  ; branch : Branch.t
  ; height : int
  }
[@@deriving compare, equal]

val compare : t -> t -> int

val view : t -> string
