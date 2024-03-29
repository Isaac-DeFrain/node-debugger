type t [@@deriving compare, equal]

val empty : t

val insert : Block.t -> t -> t

val remove : Block.t -> t -> t

val of_list : Block.t list -> t

val to_list : t -> Block.t list

val view : t -> string
