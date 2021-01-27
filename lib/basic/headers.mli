type t

val empty : t

val insert : Header.t -> t -> t

val remove : Header.t -> t -> t

val to_list : t -> Header.t list

val view : t -> string
