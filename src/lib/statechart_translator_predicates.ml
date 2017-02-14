module Bitset = Statechart_bitset
open Statechart_executable

let for_all fn arr =
  let n = Array.length arr in
  let rec find i =
    if (i < n) && (fn arr.(i))
    then find (i + 1)
    else false
  in
  find 0

let normalize_source states transition =
  let source = transition.Transition.source in
  match Array.get states source with
  | {State.t=`initial; parent} -> {transition with Transition.source=parent}
  | _ -> transition

let find_lcca descendants states source targets =
  let ancestors = source.State.ancestors in
  let targets = Array.append [|source.State.idx|] targets in
  let n = (Bitset.length ancestors) - 1 in

  let is_compound i =
    (Bitset.get ancestors i) && (
      let state = Array.get states i in
      state.State.t == `compound
    )
  in

  let rec find_ancestor i =
    if i > n then None else (
      if not (is_compound i)
      then find_ancestor (i + 1)
      else (
        let anc_desc = Array.get descendants i in
        if for_all (Bitset.get anc_desc) targets
        then Some i
        else find_ancestor (i + 1)
      )
    )
  in

  match find_ancestor 0 with
  | None -> Bitset.last ancestors
  | anc -> anc

let get_transition_domain descendants states transition targets =
  match targets with
  | [||] -> None
  | targets ->
    let idx = transition.Transition.source in
    let source = Array.get states idx in
    match transition with
    | {Transition.t=`internal} when source.State.t=`compound -> (
      let source_desc = Array.get descendants idx in
      if for_all (Bitset.get source_desc) targets
      then Some idx
      else find_lcca descendants states source targets
    )
    | _ -> find_lcca descendants states source targets

let get_exit_set descendants states transition targets =
  match get_transition_domain descendants states transition targets with
  | None -> Bitset.make (Array.length states)
  | Some idx -> (
    let desc = Array.get descendants idx in
    Bitset.filter (fun i ->
      let {State.t=t} = Array.get states i in
      t == `parallel || t == `compound || t == `atomic || t == `final
    ) desc
  )

let has_conflict descendants t1 t2 =
  let s1 = t1.Transition.source in
  let s2 = t2.Transition.source in

  if s1 == s2 then true else (
    let d1 = Array.get descendants s1 in
    let d2 = Array.get descendants s2 in
    (Bitset.has_and d1 d2) || (Bitset.has_and t1.Transition.exits t2.Transition.exits)
  )
