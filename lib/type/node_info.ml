[@@@warning "-27"]

open Basic

(* module State *)

module IdMap = Map.Make(Id)

module NC = struct
  type t = Id.t * Chain.t
  let compare (n1, c1) (n2, c2) =
    let n_cmp = compare n1 n2 in
    if n_cmp < 0 || n_cmp > 0 then n_cmp
    else compare c1 c2
end

module NCMap = Map.Make(NC)

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

(* [node] becomes active on [chain] *)
let activate info node chain =
  match IdMap.find_opt node info.active with
  | None -> info.active <- IdMap.add node [chain] info.active
  | Some chains ->
    if List.mem chain chains then ()
    else
    let updated = List.sort_uniq compare (chain :: chains) in
    info.active <- IdMap.add node updated info.active

let ack info node chain (* msg *) = ()

let advertise info node chain (* adv_type *) = ()

let handle info node chain message = ()

let request info node chain (* req_type *) = ()

let err info node chain (* err_type *) = ()

let view_node info node = "TODO"

let view info =
  let node_id n = String.make 2 ' ' ^ Id.view n ^ " :> " in
  let chain_id c = String.make 4 ' ' ^ Chain.view c ^ " :> " in
  let branch_id b = String.make 6 ' ' ^ Branch.view b ^ " :> " in
  String.concat "\n"
    [ "nodes: " ^ "[" ^ Array.fold_left (fun a b -> if a = "" then a ^ b else a ^ ", " ^ b) ""
      (Array.map Id.view info.nodes) ^ "]"
    ; "active:\n" ^
      String.concat "\n"
        (Array.to_list (Array.map (fun n ->
          node_id n ^ "[" ^ String.concat ", "
          (List.map Chain.view (try IdMap.find n info.active with Not_found -> [])) ^ "]")
        info.nodes))
    ; "blocks:\n" ^
      String.concat "\n"
        (Array.to_list (Array.map (fun n ->
          node_id n ^ String.concat "\n"
            (List.map (fun c ->
            chain_id c ^ String.concat "\n" (List.map (fun b ->
              branch_id b ^ "[" ^ String.concat ", "
                (List.map Block.view
                  (try Blocks.to_list (NCBMap.find (n, c, b) info.blocks)
                   with Not_found -> [])) ^ "]")
              (try NCMap.find (n, c) info.branches with Not_found -> [])))
            (try IdMap.find n info.active with Not_found -> [])))
        info.nodes))
    ; "branches:\n" ^
      String.concat "\n"
        (Array.to_list (Array.map (fun n ->
          node_id n ^ String.concat "\n" (List.map (fun c ->
            chain_id c ^ "[" ^
            String.concat ", " (List.map Branch.view
              (try NCMap.find (n, c) info.branches with Not_found -> [])) ^ "]")
            (try IdMap.find n info.active with Not_found -> [])))
        info.nodes))
    ; "expect:\n" ^
      String.concat "\n"
        (Array.to_list (Array.map (fun n ->
          node_id n ^ String.concat "\n" (List.map (fun c ->
            chain_id c ^ "[" ^
            String.concat ", " (List.map Message.view
              (try Messages.to_list (NCMap.find (n, c) info.expect) with Not_found -> [])) ^ "]")
            (try IdMap.find n info.active with Not_found -> [])))
        info.nodes))
    ; "headers:\n" ^
      String.concat "\n"
        (Array.to_list (Array.map (fun n ->
          node_id n ^ String.concat "\n" (List.map (fun c ->
            chain_id c ^ "[" ^
            String.concat ", " (List.map Header.view
              (try Headers.to_list (NCMap.find (n, c) info.headers) with Not_found -> [])) ^ "]")
            (try IdMap.find n info.active with Not_found -> [])))
        info.nodes))
    ; "height:\n" ^
      String.concat "\n"
        (Array.to_list (Array.map (fun n ->
          node_id n ^ String.concat "\n"
            (List.map (fun c ->
              chain_id c ^ "[" ^
                String.concat ", " (List.map (fun b ->
                  (try string_of_int ((NCBMap.find (n, c, b) info.height))
                  with Not_found -> "-1"))
                (try NCMap.find (n, c) info.branches with Not_found -> [])) ^ "]")
              (try IdMap.find n info.active with Not_found -> [])))
        info.nodes))
    ; "messages:\n" ^
      String.concat "\n"
        (Array.to_list (Array.map (fun n ->
          node_id n ^ String.concat "\n" (List.map (fun c ->
            chain_id c ^ "[" ^
            String.concat ", " (List.map Message.view
              (try Messages.to_list (NCMap.find (n, c) info.messages) with Not_found -> [])) ^ "]")
            (try IdMap.find n info.active with Not_found -> [])))
        info.nodes))
    ]

    (* (Array.to_list (Array.map (fun n ->
          node_id n ^ String.concat "\n"
            (List.map (fun c ->
              chain_id c ^ "[" ^
              String.concat ", " (List.map (fun b ->
                (try string_of_int ((NCBMap.find (n, c, b) info.height))
                with Not_found -> "-1"))
              (try NCMap.find (n, c) info.branches with Not_found -> [])) ^ "]"
            (try IdMap.find n info.active with Not_found -> [])))
        info.nodes)) *)