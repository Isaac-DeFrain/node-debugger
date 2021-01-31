module type Type = sig
  type t

  val compare : t -> t -> int

  val view : t -> string
end

module type IdType = sig
  include Type

  val id : int -> t

  val to_int : t -> int
end
