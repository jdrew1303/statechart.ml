open Statechart
open Statechart_event

type configuration = state list

module EnterState = struct
  type t = state
  let compare = Statechart.compare_state
end
module EnterStates = Set.Make(EnterState)

module ExitState = struct
  type t = state
  let compare = Statechart.compare_state_reverse
end
module ExitStates = Set.Make(ExitState)

module type ContextType =
  sig
    type t
    val update_configuration : t -> configuration -> t
    val get_configuration : t -> configuration
    val query : t -> expression -> bool
    val execute : t -> expression -> t
    val invoke : t -> invoke -> t
    val cancel_invoke : t -> invoke -> t
    val stop : t -> t
  end

module type Interpreter =
  sig
    type t
    val start : t -> document -> t
    val handle_event : t -> document -> Statechart_event.t -> t
    val stop : t -> t
  end

let resolve doc idx =
  Array.get doc.Document.states idx

let compute_exit_set doc transitions conf =
  [], conf

let has_processed_children doc state processed =
  List.exists (fun idx -> EnterStates.mem (resolve doc idx) processed) state.State.children

let query_ancestors doc target scope =
  []

let rec add_state_and_ancestors doc history scope acc target =
  let state = resolve doc target in
  let acc = add_descendants doc history state acc in
  maybe_add_ancestors doc history scope acc state.State.ancestors

and maybe_add_ancestors doc history scope acc ancestors =
  match ancestors with
  | anc :: ancestors when anc != scope ->
    add_state_and_ancestors doc history scope acc anc
  | _ -> acc

and add_descendants doc history state acc =
  let states, conf, processed = acc in
  match EnterStates.mem state processed with
  | true -> acc
  | false -> add_descendants_dispatch doc history state acc

and add_descendants_dispatch doc history state acc =
  let states, conf, processed = acc in
  let processed = EnterStates.add state processed in
  let acc = states, conf, processed in
  match state.State.type_ with
  | `history -> add_history_descendants doc history state acc
  | `parallel -> add_parallel_descendants doc history state acc
  | `composite -> add_composite_descendants doc history state acc
  | _ -> add_other_descendants doc history state acc

and add_history_descendants doc history state acc =
  (* TODO *)
  (* let states, conf, processed = acc in *)
  acc

and add_parallel_descendants doc history state acc =
  let states, conf, processed = acc in
  let states = EnterStates.add state states in
  let acc = states, conf, processed in
  List.fold_left (add_parallel_descendants_child doc history) acc state.State.children

and add_parallel_descendants_child doc history acc idx =
  let child = resolve doc idx in
  match child.State.type_ with
  | `history -> acc
  | _ -> add_descendants doc history child acc

and add_composite_descendants doc history state acc =
  let states, conf, processed = acc in
  let states = EnterStates.add state states in
  let acc = states, conf, processed in
  match has_processed_children doc state processed with
  | true -> acc
  | _ ->
    match state.State.initial_state with
    | Some idx ->
      let initial = resolve doc idx in
      add_descendants doc history initial acc
    | None ->
      acc

and add_other_descendants doc history state acc =
  let states, conf, processed = acc in
  let states = EnterStates.add state states in
  let conf = state :: conf in
  states, conf, processed

let compute_entry_set doc history transitions conf =
  let init = EnterStates.empty, conf, EnterStates.empty in
  let enter_state acc transition =
    let targets = transition.Transition.targets in
    let scope = transition.Transition.scope in
    List.fold_left (add_state_and_ancestors doc history scope) acc targets
  in
  let states, conf, _ = List.fold_left enter_state init transitions in
  states, conf

module Make(Ctx : ContextType) =
  struct
    type t = Ctx.t

    let execute_all = List.fold_left Ctx.execute

    let cancel_invoke_all = List.fold_left Ctx.cancel_invoke

    let exit_state context state =
      let context = execute_all context state.State.on_exit in
      cancel_invoke_all context state.State.invocations

    let exit_states context doc transitions conf =
      let states, conf = compute_exit_set doc transitions conf in
      (* TODO support history *)
      let context = List.fold_left exit_state context states in
      context, conf

    let exec_transition context transition =
      execute_all context transition.Transition.on_transition

    let exec_transitions = List.fold_left exec_transition

    let enter_states context doc transitions conf =
      let history = [] in (* TODO figure out history *)
      let states, conf = compute_entry_set doc history transitions conf in
      context, conf

    let microstep context doc transitions =
      let conf          = Ctx.get_configuration context in
      let context, conf = exit_states context doc transitions conf in
      let context       = exec_transitions context transitions in
      let context, conf = enter_states context doc transitions conf in
      Ctx.update_configuration context conf

    let start context doc =
      let conf          = [] in
      let initial       = doc.Document.initial_transitions in
      let context, conf = enter_states context doc initial conf in
      Ctx.update_configuration context conf

    let handle_event context doc event =
      (* TODO *)
      let transitions = [] in
      microstep context doc transitions

    let stop context =
      let conf = Ctx.get_configuration context in
      let states = conf in (* TODO get states from configuration *)
      let context = List.fold_left exit_state context states in
      Ctx.stop context
  end
