type t

val empty : t

val length : t -> int

val add : Message.t -> t -> t

val remove : Message.t -> t -> t

val to_list : t -> Message.t list

val of_list : Message.t list -> t

val view : t -> string
