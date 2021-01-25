[@@@warning "-37"]

module type NODE = sig type t end
module type ID = sig type t val id : t end
module Node(Id : ID) : NODE = struct
  [@@@warning "-32"]
  type t
  let id = Id.id
end

type node

module type ViewableType = sig
  type t
  val compare : t -> t -> int
  val view : t -> string
end

module type ViewableIdType = sig
  type t
  val compare : t -> t -> int
  val id : int -> t
  val to_int : t -> int
  val view : t -> string
end

module Id : ViewableIdType = struct
  type t = int
  let id n = n
  let compare = compare
  let to_int n = n
  let view = string_of_int
end

type id = Id.t

module Chain : ViewableIdType = struct
  type t = int
  let id c = c
  let compare = compare
  let to_int c = c
  let view = string_of_int
end

type chain = Chain.t

module Branch : ViewableIdType = struct
  type t = int
  let id b = b
  let compare x y = compare y x
  let to_int b = b
  let view = string_of_int
end

type branch = Branch.t

module Header = struct
  type t = { chain : Chain.t; branch : Branch.t; height: int }

  let header chain branch height = {chain; branch; height}

  let compare h1 h2 =
    let cmp_chain = Chain.compare h1.chain h2.chain in
    let cmp_branch = Branch.compare h1.branch h2.branch in
    let cmp_height = compare h1.height h2.height in
    if cmp_chain < 0 || cmp_chain > 0 then cmp_chain
    else if cmp_branch < 0 || cmp_branch > 0 then cmp_branch
    else cmp_height

  let view h = "Header(" ^
    String.concat ", "
      [ "chain: " ^ Chain.view h.chain
      ; "branch: " ^ Branch.view h.branch
      ; "height: " ^ string_of_int h.height
      ] ^ ")"
end

type header = Header.t

module type HEADERS = sig
  type t
  val empty : t
  val insert : Header.t -> t -> t
  val remove : Header.t -> t -> t
  val to_list : t -> Header.t list
  val view : t -> string
end

module Headers : HEADERS = struct
  type t = Header.t list

  let empty : t = []

  let insert h = function
  | [] -> [h]
  | hs -> h::hs |> List.sort_uniq Header.compare

  let rec remove h = function
  | [] -> []
  | hd::tl as hs ->
    let cmp = compare h hd in  
    if cmp = 0 then tl
    else if cmp < 0 then hs
    else hd :: remove h tl

  let to_list hs = hs

  let view = function
  | [] -> "[]"
  | hs -> "[" ^ String.concat ", " (List.map Header.view hs) ^ "]"
end

type headers = Headers.t

module Ops = struct
  type t = int * int

  let ops h n = (h, n)

  let compare (h1, n1) (h2, n2) = 
    let cmp_height = compare h1 h2 in
      if cmp_height < 0 || cmp_height > 0 then cmp_height
      else compare n1 n2

  let view (h, n) = "(" ^ string_of_int h ^ ", " ^ string_of_int n ^ ")"
end

type ops = Ops.t

module Block = struct
  type t = { header : Header.t; ops : Ops.t }

  let block header ops = {header; ops}

  let compare b1 b2 = Header.compare b1.header b2.header
  let view b = "Block(" ^ Header.view b.header ^ ", ops: " ^ Ops.view b.ops ^ ")"
end

type block = Block.t

module type BLOCKS = sig
  type t
  val empty : t
  val insert : Block.t -> t -> t
  val remove : Block.t -> t -> t
  val to_list : t -> Block.t list
  val view : t -> string
end

module Blocks : BLOCKS = struct
  type t = Block.t list

  let empty : t = []

  let insert b = function
  | [] -> [b]
  | bs -> b::bs |> List.sort_uniq Block.compare

  let rec remove b = function
  | [] -> []
  | hd::tl as bs ->
    let cmp = compare b hd in  
    if cmp = 0 then tl
    else if cmp < 0 then bs
    else hd :: remove b tl

  let to_list bs = bs

  let view = function
  | [] -> "[]"
  | bs -> "[" ^ String.concat ", " (List.map Block.view bs) ^ "]"
end

type blocks = Blocks.t

module Message : ViewableType = struct
  type t = Id.t * Id.t * string

  let compare (f1, t1, m1) (f2, t2, m2) =
    let from_cmp = Id.compare f1 f2 in
    let to_cmp = Id.compare t1 t2 in
    if from_cmp < 0 || from_cmp > 0 then from_cmp
    else if to_cmp < 0 || to_cmp > 0 then to_cmp
    else compare m1 m2

  let view (sender, receiver, msg) =
    String.concat ""
      [ "Msg(from: "; Id.view sender; ", "; "to: "; Id.view receiver; ", "; msg; ")" ]
end

type message = Message.t

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

module Messages : MESSAGES = struct
  type t = Message.t Queue.t

  open Queue
  let create = create

  let empty = create ()

  let length = length

  let clear = clear

  let push = push

  let pop = pop

  let peek = peek_opt

  let is_empty = is_empty

  let to_list msgs =
    let cpy = copy msgs in
    let rec elems q =
      if is_empty q then []
      else let p = pop q in p :: elems q
    in
    elems cpy

  let view msgs =
    let cpy = copy msgs in
    let rec view_elems q =
      if is_empty q then []
      else
      let p = pop q in
      Message.view p :: view_elems q
    in
    "[" ^ String.concat ", " (view_elems cpy) ^ "]"
end

type messages = Messages.t

let node = Id.id

let chain = Chain.id

let branch = Branch.id

module List = struct
  include List
  let remove elem = function
  | [] -> []
  | l -> List.filter (fun x -> x <> elem) l
end
