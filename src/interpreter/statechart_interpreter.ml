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
    val init_datamodel : t -> datamodel -> t
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
    val init : t -> document -> t
    val start : t -> document -> t
    val handle_event : t -> document -> Statechart_event.t -> t
    val stop : t -> t
  end

let resolve doc idx =
  Array.get doc.Document.states idx

let exit_states_ doc transitions conf =
  [], conf

let enter_states_ doc history transitions conf =
  [], conf

module Make(Ctx : ContextType) =
  struct
    type t = Ctx.t

    let init context doc =
      match doc.Document.binding with
      | `early -> Ctx.init_datamodel context doc.Document.datamodel
      | `late -> context

    let exit_states context doc transitions conf =
      let states, conf = exit_states_ doc transitions conf in
      context, conf

    let exec_transitions context doc transitions =
      context

    let enter_states context doc transitions conf =
      let history = [] in (* TODO figure out history *)
      let states, conf = enter_states_ doc history transitions conf in
      context, conf

    let microstep context doc transitions =
      let conf          = Ctx.get_configuration context in
      let context, conf = exit_states context doc transitions conf in
      let context       = exec_transitions context doc transitions in
      let context, conf = enter_states context doc transitions conf in
      Ctx.update_configuration context conf

    let start context doc =
      let conf          = [] in
      let initial       = [doc.Document.initial_transition] in
      let context, conf = enter_states context doc initial conf in
      Ctx.update_configuration context conf

    let handle_event context doc event =
      (* TODO *)
      start context doc

    let execute_all = List.fold_left Ctx.execute

    let cancel_invoke_all = List.fold_left Ctx.cancel_invoke

    let stop_state context state =
      let context = execute_all context state.State.on_exit in
      let context = cancel_invoke_all context state.State.invocations in
      Ctx.stop context

    let stop context =
      let conf = Ctx.get_configuration context in
      let states = conf in (* TODO get states from configuration *)
      List.fold_left stop_state context states
  end
