[@@@warning "-27"]

open Basic

(* module State *)

(* node ids *)
module IdMap = Map.Make(Id)

(* parameters: node, chain *)
module NC = struct
  type t = Id.t * Chain.t
  let compare (n1, c1) (n2, c2) =
    let n_cmp = compare n1 n2 in
    if n_cmp < 0 || n_cmp > 0 then n_cmp
    else compare c1 c2
end

module NCMap = Map.Make(NC)

(* parameters: node, chain, branch *)
module NCB = struct
  type t = Id.t * Chain.t * Branch.t
  let compare (n1, c1, b1) (n2, c2, b2) =
    let n_cmp = compare n1 n2 in
    let c_cmp = compare c1 c2 in
    if n_cmp < 0 || n_cmp > 0 then n_cmp
    else if c_cmp < 0 || c_cmp > 0 then c_cmp
    else compare b1 b2
end

module NCBMap = Map.Make(NCB)

(* complete state of all nodes *)
type t = {
  nodes : id array;
  mutable active   : chain list IdMap.t;
  mutable blocks   : Blocks.t NCBMap.t;
  mutable branches : branch list NCMap.t;
  mutable expect   : messages NCMap.t;
  mutable headers  : headers NCMap.t;
  mutable height   : int NCBMap.t;
  mutable messages : messages NCMap.t;
}

let init num_nodes =
  let nodes = Array.init num_nodes Id.id in
  {
    nodes;
    active   = IdMap.empty;
    blocks   = NCBMap.empty;
    branches = NCMap.empty;
    expect   = NCMap.empty;
    headers  = NCMap.empty;
    height   = NCBMap.empty;
    messages = NCMap.empty;
}

let node = Id.id

let chain = Chain.id

let branch = Branch.id

let ack info node chain (* msg *) = ()

let advertise info node chain (* adv_type *) = ()

let handle info node chain message = ()

let request info node chain (* req_type *) = ()

let err info node chain (* err_type *) = ()

let active info node =
  try IdMap.find node info.active
  with Not_found -> []

let blocks info node chain branch =
  try NCBMap.find (node, chain, branch) info.blocks
  with Not_found -> Blocks.empty

let branches info node chain =
  try NCMap.find (node, chain) info.branches
  with Not_found -> []

let expect info node chain =
  try NCMap.find (node, chain) info.expect
  with Not_found -> Messages.empty

let headers info node chain =
  try NCMap.find (node, chain) info.headers
  with Not_found -> Headers.empty

let height info node chain branch =
  try NCBMap.find (node, chain, branch) info.height
  with Not_found -> -1

let messages info node chain =
  try NCMap.find (node, chain) info.messages
  with Not_found -> Messages.empty

(* viewing functions *)
let view_nodes info =
  "[" ^ Array.fold_left (fun a b -> if a = "" then a ^ b else a ^ ", " ^ b) ""
    (Array.map Id.view info.nodes) ^ "]"

let view_active info node =
  "[" ^ String.concat ", " (List.map Chain.view (active info node)) ^ "]"

let view_blocks info node chain branch =
  "[" ^ String.concat ", "
  (List.map Block.view (blocks info node chain branch |> Blocks.to_list)) ^ "]"

let view_branches info node chain =
  "[" ^ String.concat ", " (List.map Branch.view (branches info node chain)) ^ "]"

let view_expect info node chain =
  "[" ^ String.concat ", "
  (List.map Message.view (expect info node chain |> Messages.to_list)) ^ "]"

let view_headers info node chain =
  "[" ^ String.concat ", "
  (List.map Header.view (headers info node chain |> Headers.to_list)) ^ "]"

let view_heights info node chain =
  "(" ^ String.concat ", "
  (List.map (fun b -> (Branch.view b ^ " :> " ^ string_of_int (height info node chain b)))
    (branches info node chain)) ^ ")"

let view_messages info node chain =
  "[" ^ String.concat ", "
  (List.map Message.view (messages info node chain |> Messages.to_list)) ^ "]"

(* view of [node]'s state *)
let view_node info node =
  let chain_id c = String.make 2 ' ' ^ Chain.view c ^ " :> " in
  let branch_id b = String.make 4 ' ' ^ Branch.view b ^ " :> " in
  String.concat "\n"
    [ "node: " ^ Id.view node
    ; "active: " ^ view_active info node
    ; "blocks:\n" ^ String.concat "\n"
      (List.map (fun c ->
        chain_id c ^ String.concat "\n"
        (let l = List.map (fun b -> branch_id b ^ view_blocks info node c b) (branches info node c) in
        if l = [] then l else "" :: List.rev l))
      (active info node))
    ; "branches:\n" ^ String.concat "\n"
      (List.map (fun c -> chain_id c ^ view_branches info node c) (active info node))
    ; "expect:\n" ^ String.concat "\n"
      (List.map (fun c -> chain_id c ^ view_expect info node c) (active info node))
    ; "headers:\n" ^ String.concat "\n"
      (List.map (fun c -> chain_id c ^ view_headers info node c) (active info node))
    ; "height:\n" ^ String.concat "\n"
      (List.map (fun c -> chain_id c ^ view_heights info node c) (active info node))
    ; "messages:\n" ^ String.concat "\n"
      (List.map (fun c -> chain_id c ^ view_messages info node c) (active info node))
    ]

(* view of complete state of all nodes *)
let view info =
  let node_id n = String.make 2 ' ' ^ Id.view n ^ " :> " in
  let chain_id c = String.make 4 ' ' ^ Chain.view c ^ " :> " in
  let branch_id b = String.make 6 ' ' ^ Branch.view b ^ " :> " in
  String.concat "\n"
    [ "nodes: " ^ view_nodes info
    ; "active:\n" ^ String.concat "\n"
        (Array.to_list (Array.map (fun n -> node_id n ^ view_active info n)
        info.nodes))
    ; "blocks:\n" ^ String.concat "\n"
        (Array.to_list (Array.map (fun n ->
          node_id n ^ String.concat "\n"
            (List.map (fun c ->
              chain_id c ^ String.concat "\n"
                (let l = List.map (fun b ->
                  branch_id b ^ view_blocks info n c b) (branches info n c)
                in
                if l = [] then l else "" :: List.rev l))
            (active info n)))
        info.nodes))
    ; "branches:\n" ^ String.concat "\n"
        (Array.to_list (Array.map (fun n ->
          node_id n ^ String.concat "\n"
          (List.map (fun c -> chain_id c ^ view_branches info n c) (active info n)))
        info.nodes))
    ; "expect:\n" ^ String.concat "\n"
        (Array.to_list (Array.map (fun n ->
          node_id n ^ String.concat "\n"
          (List.map (fun c -> chain_id c ^ view_expect info n c) (active info n)))
        info.nodes))
    ; "headers:\n" ^ String.concat "\n"
        (Array.to_list (Array.map (fun n ->
          node_id n ^ String.concat "\n"
            (List.map (fun c -> chain_id c ^ view_headers info n c) (active info n)))
        info.nodes))
    ; "height:\n" ^ String.concat "\n"
        (Array.to_list (Array.map (fun n ->
          node_id n ^ String.concat "\n"
            (List.map (fun c -> chain_id c ^ view_heights info n c) (active info n)))
        info.nodes))
    ; "messages:\n" ^ String.concat "\n"
        (Array.to_list (Array.map (fun n ->
          node_id n ^ String.concat "\n"
          (List.map (fun c -> chain_id c ^ view_messages info n c) (active info n)))
        info.nodes))
    ]

(* [node] becomes active on [chain] *)
let activate info node chain =
  if not (Array.mem node info.nodes)
  then Printf.printf "%s\n"
    (Id.view node ^ " is not in the set of nodes " ^ view_nodes info)
  else
  match IdMap.find_opt node info.active with
  | None -> info.active <- IdMap.add node [chain] info.active
  | Some chains ->
    if List.mem chain chains then Printf.printf "%s\n"
      ("node " ^ Id.view node ^ " is already active on chain " ^ Chain.view chain)
    else
    let updated = List.sort_uniq compare (chain :: chains) in
    info.active <- IdMap.add node updated info.active
