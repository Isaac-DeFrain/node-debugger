module type ViewableType = Viewable.Type

module type ViewableIdType = Viewable.IdType

module Block = Block
module Blocks = Blocks
module Branch = Branch
module Chain = Chain
module Header = Header
module Headers = Headers
module Id = Id
module Message = Message
module Messages = Messages
module Msg = Msg
module Ops = Ops

module List : sig
  include module type of Base.List

  val compare_list : 'a list -> 'a list -> int

  val remove_all : 'a -> 'a t -> 'a t

  val remove_one : 'a -> 'a t -> 'a t

  val remove_list : 'a t -> 'a t -> 'a t

  val random_elem : 'a t -> 'a
end

module Queue : sig
  include module type of Base.Queue

  val to_list : 'a t -> 'a list

  val of_list : 'a list -> 'a t
end

module String : sig
  include module type of Base.String

  val concat_comma : string list -> string

  val concat_endline : string list -> string

  val concat_endline2 : string list -> string

  val concat_endline4 : string list -> string
end

val ( @@ ) : ('a -> 'b) -> 'a -> 'b

val compare_int : int -> int -> int

val equal_int : int -> int -> bool

val compare_list : 'a list -> 'a list -> int

val print : ('a -> string) -> 'a -> unit
