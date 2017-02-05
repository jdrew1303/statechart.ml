open Statechart_analyzer_types
module SC = Statechart

let map_id map id idx =
  match id with
  | Some id -> StateIDMap.add id idx map
  | _ -> map

let rec assign_idx_rec node idx map ancs =
  match node with
  | State s ->
    let s = {s with State.idx=Some idx; ancestors=ancs} in
    let map = map_id map s.State.id idx in
    let children, idx, map = assign_children s.State.children idx map ancs in
    State {s with State.children=children}, idx, map
  | Parallel s ->
    let s = {s with Parallel.idx=Some idx; ancestors=ancs} in
    let map = map_id map s.Parallel.id idx in
    let children, idx, map = assign_children s.Parallel.children idx map ancs in
    Parallel {s with Parallel.children=children}, idx, map
  | Initial s ->
    let s = {s with Initial.idx=Some idx; ancestors=ancs} in
    let children, idx, map = assign_children s.Initial.children idx map ancs in
    Initial {s with Initial.children=children}, idx, map
  | Final s ->
    let s = {s with Final.idx=Some idx; ancestors=ancs} in
    let map = map_id map s.Final.id idx in
    let children, idx, map = assign_children s.Final.children idx map ancs in
    Final {s with Final.children=children}, idx, map
  | History s ->
    let s = {s with History.idx=Some idx; ancestors=ancs} in
    let map = map_id map s.History.id idx in
    let children, idx, map = assign_children s.History.children idx map ancs in
    History {s with History.children=children}, idx, map
  | _ -> node, idx, map

and assign_children children parent map ancs =
  let ancs = parent :: ancs in
  let idx = parent + 1 in
  let c, idx, map = List.fold_left (fun acc child ->
    let c, idx, map = acc in
    let child, idx, map = assign_idx_rec child idx map ancs in
    let c = child :: c in
    c, (idx + 1), map
  ) ([], idx, map) children in
  (List.rev c), idx, map

let assign_state_ids doc =
  let map = StateIDMap.empty in
  let children, count, map = assign_children doc.Document.children (-1) map [] in
  {doc with Document.children=children; state_map=Some map; state_count=count}

let analyze doc =
  assign_state_ids doc
