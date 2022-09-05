type t [@@deriving compare, equal]

val empty : t

val insert : Header.t -> t -> t

val remove : Header.t -> t -> t

val of_list : Header.t list -> t

val to_list : t -> Header.t list

val view : t -> string
