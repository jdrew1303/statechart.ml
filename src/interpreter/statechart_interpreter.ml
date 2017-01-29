open Statechart
open Statechart_event

type configuration = state list

module EnterTransition = struct
  type t = transition
  let compare = Statechart.compare_transition
end
module EnterTransitions = Set.Make(EnterTransition)

module ExitTransition = struct
  type t = transition
  let compare = Statechart.compare_transition_reverse
end
module ExitTransitions = Set.Make(ExitTransition)

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

let compute_entry_set doc history transitions conf =
  [], conf

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
      let initial       = [doc.Document.initial_transition] in
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
