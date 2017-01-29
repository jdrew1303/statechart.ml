open Statechart
open Statechart_event

module State = struct
  type t = state
  let compare = Statechart.compare_state
end
module Configuration = Set.Make(State)

type configuration = State

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
    val stop : t -> t
  end

module type Interpreter =
  sig
    type t
    val start : t -> document -> t
    val handle_event : t -> document -> Statechart_event.t -> t
    val stop : t -> t
  end

module Make(Ctx : ContextType) =
  struct
    type t = Ctx.t

    let exit_states context doc transitions =
      context

    let execute_transitions context doc transitions =
      context

    let enter_states context doc transitions =
      context

    let microstep context doc transitions =
      context
      |> exit_states doc transitions
      |> execute_transitions doc transitions
      |> enter_states doc transitions

    let start context doc =
      enter_states context doc [doc.Document.initial_transition]

    let handle_event context doc event =
      enter_states context doc [doc.Document.initial_transition]

    let stop context =
      context
  end
