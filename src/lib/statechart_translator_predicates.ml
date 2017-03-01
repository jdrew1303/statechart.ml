open Statechart_executable
module Bitset = Statechart_bitset

let for_all fn arr =
  let n = Array.length arr in
  let rec loop i =
    if i = n then true
    else if fn arr.(i) then loop (succ i)
    else false
  in
  loop 0

let for_any fn arr =
  let n = Array.length arr in
  let rec loop i =
    if i = n then false
    else if fn arr.(i) then true
    else loop (succ i)
  in
  loop 0

let filter_state_type bitset states fn =
  Bitset.filter (fun i ->
    let {State.t=t} = Array.get states i in
    fn t
  ) bitset

let resolve_transition_type states transition =
  match Array.get states transition.Transition.source with
  | {State.t=`initial; parent} ->
    {transition with Transition.t=`initial}
  | {State.t} when t == `history_deep || t == `history_shallow ->
    {transition with Transition.t=`history}
  | _ ->
    transition

let are_descendants descendants i ids =
  let desc = Array.get descendants i in
  for_all (Bitset.get desc) ids

let find_lcca descendants states source targets =
  let {State.idx; ancestors} = source in
  let ancestors = filter_state_type ancestors states (fun t ->
    t == `parallel || t == `compound || t == `atomic
  ) in
  let targets = Array.append [|idx|] targets in

  let is_compound i =
    let state = Array.get states i in
    state.State.t == `compound
  in

  let is_lcca i =
    (is_compound i) && (are_descendants descendants i targets)
  in

  match Bitset.find_right is_lcca ancestors with
  | None -> Bitset.first ancestors
  | anc -> anc

let get_transition_source states transition =
  let idx = transition.Transition.source in
  let source = Array.get states idx in
  match source with
  | {State.t=`initial; parent} -> parent, (Array.get states parent)
  | _ -> idx, source

let get_transition_domain descendants states transition targets =
  match targets with
  | [||] -> None
  | targets ->
    let idx, source = get_transition_source states transition in
    match transition with
    | {Transition.t=`internal} when source.State.t=`compound -> (
      if are_descendants descendants idx targets
      then Some idx
      else find_lcca descendants states source targets
    )
    | _ -> find_lcca descendants states source targets

let get_exit_set descendants states transition targets =
  match get_transition_domain descendants states transition targets with
  | None -> Bitset.make (Array.length states)
  | Some idx ->
    let desc = Array.get descendants idx in
    filter_state_type desc states (fun t ->
      t == `parallel || t == `compound || t == `atomic || t == `final
    )

let has_conflict descendants states t1 t2 =
  let s1, _ = get_transition_source states t1 in
  let s2, _ = get_transition_source states t2 in

  s1 == s2 ||
  (Bitset.has_and t1.Transition.exits t2.Transition.exits) ||
  (
    let d1 = Array.get descendants s1 in
    let d2 = Array.get descendants s2 in
    (Bitset.get d1 s2) || (Bitset.get d2 s1)
  )
