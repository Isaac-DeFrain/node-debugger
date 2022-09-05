module type Type = sig
  type t [@@deriving compare]

  val view : t -> string
end

module type IdType = sig
  include Type

  val id : int -> t

  val to_int : t -> int
end
