open Statechart_analyzer_types

let rec assign_idx_rec node idx ancs =
  match node with
  | State s ->
    let s = {s with State.idx=Some idx; ancestors=ancs} in
    let children, idx, ancs = assign_children s.State.children idx ancs in
    State {s with State.children=children}, idx, ancs
  | Parallel s ->
    let s = {s with Parallel.idx=Some idx; ancestors=ancs} in
    let children, idx, ancs = assign_children s.Parallel.children idx ancs in
    Parallel {s with Parallel.children=children}, idx, ancs
  | Initial s ->
    let s = {s with Initial.idx=Some idx; ancestors=ancs} in
    let children, idx, ancs = assign_children s.Initial.children idx ancs in
    Initial {s with Initial.children=children}, idx, ancs
  | History s ->
    let s = {s with History.idx=Some idx; ancestors=ancs} in
    let children, idx, ancs = assign_children s.History.children idx ancs in
    History {s with History.children=children}, idx, ancs
  | _ -> node, idx, ancs

and assign_children children parent ancs =
  let ancs = parent :: ancs in
  let idx = parent + 1 in
  let c, idx, ancs = List.fold_left (fun acc child ->
    let c, idx, ancs = acc in
    let child, idx, ancs = assign_idx_rec child idx ancs in
    let c = child :: c in
    c, (idx + 1), ancs
  ) ([], idx, ancs) children in
  (List.rev c), idx, ancs

let assign_state_ids doc =
  let children, _, _ = assign_children doc.Document.children (-1) [] in
  {doc with Document.children=children}
