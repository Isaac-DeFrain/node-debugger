module type NODE
module type ID = sig type t val id : t end
module Node(Id : ID) : NODE

module type ViewableType = sig
  type t
  val compare : t -> t -> int
  val view : t -> string
end

module type ViewableIdType = sig
  type t
  val compare : t -> t -> int
  val id : int -> t
  val view : t -> string
end

module Id : ViewableIdType
module Branch : ViewableIdType
module Chain : ViewableIdType

module Header : sig
  include ViewableType
  val header : Chain.t -> Branch.t -> int -> t
end

module Ops : sig 
  include ViewableType
  val ops : int -> int -> t
end

module Block : sig
  include ViewableType
  val block : Header.t -> Ops.t -> t
end

module type HEADERS = sig
  type t
  val empty : t
  val insert : Header.t -> t -> t
  val remove : Header.t -> t -> t
  val to_list : t -> Header.t list
  val view : t -> string
end

module Headers : HEADERS

module type BLOCKS = sig
  type t
  val empty : t
  val insert : Block.t -> t -> t
  val remove : Block.t -> t -> t
  val to_list : t -> Block.t list
  val view : t -> string
end

module Blocks : BLOCKS

module Message : ViewableType

module type MESSAGES = sig
  type t
  val empty : t
  val create : unit -> t
  val length : t -> int
  val clear : t -> unit
  val push : Message.t -> t -> unit
  val pop : t -> Message.t
  val peek : t -> Message.t option
  val is_empty : t -> bool
  val to_list : t -> Message.t list
  val view : t -> string
end

module Messages : MESSAGES

type branch = Branch.t
type chain = Chain.t
type message = Message.t
type messages = Messages.t
type header = Header.t
type headers = Headers.t
type block = Block.t
type blocks = Blocks.t
type ops = Ops.t
type id = Id.t
type node
