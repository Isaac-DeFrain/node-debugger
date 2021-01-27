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
  include module type of List
  val remove : 'a -> 'a t -> 'a t
end

module Queue : sig
  include module type of Queue
  val to_list : 'a t -> 'a list
  val of_list : 'a list -> 'a t
end
