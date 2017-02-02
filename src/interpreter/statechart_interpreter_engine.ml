module IntSet = Set.Make(struct
  type t = int
  let compare = compare
end)

module type Engine = sig
  type t
  type executable
  type idx = int
  type configuration = IntSet.t
  type event = string
  type history = int array

  module rec TYPES:
    sig
      type state_type =
        [
          | `compound
          | `basic
          | `parallel
          | `history
          | `initial
          | `final
        ]
      type history_type =
        [
          | `shallow
          | `deep
        ]
      type transition_type =
        [
          | `external_
          | `internal
        ]
      type document = Document.t
      type invoke = Invoke.t
      type param = Param.t
      type state = State.t
      type transition = Transition.t
    end
  and Document:
    sig
      type t = {
        name: string option;
        initial_transitions: TYPES.transition list;
        states: TYPES.state array;
      }
    end
  and Invoke:
    sig
      type t = {
        t: executable option;
        src: executable option;
        id: executable option;
        namelist: executable array;
        autoforward: bool;
        params: TYPES.param array;
        content: executable option;
        on_exit: executable array;
      }
    end
  and Param:
    sig
      type t = {
        id: string;
        expression: executable option;
      }
    end
  and State:
    sig
      type t = {
        idx: idx;
        depth: int;
        order: int;
        id: string option;
        t: TYPES.state_type;
        initial_state: idx option;
        transitions: TYPES.transition array;
        invocations: TYPES.invoke array;
        on_enter: executable array;
        on_exit: executable array;
        children: IntSet.t;
        parent: idx option;
        ancestors: IntSet.t;
        descendants: IntSet.t;
        history: idx array;
        history_type: TYPES.history_type option;
        donedata: executable option;
      }
    end
  and Transition:
    sig
      type t = {
        domain: idx;
        depth: int;
        order: int;
        source: idx option;
        targets: idx array option;
        events: string array option;
        condition: executable option;
        t: TYPES.transition_type;
        on_transition: executable array;
      }
    end
  and Var:
    sig
      type t = {
        name: string;
      }
    end

  type document = Document.t
  type invoke = Invoke.t
  type param = Param.t
  type state = State.t
  type transition = Transition.t

  val match_event : string array -> string -> bool
  val query : t -> executable -> bool
  val execute : t -> executable -> t
  val send : t -> event -> t
  val send_internal : t -> event -> executable option -> t
  val invoke : t -> TYPES.invoke -> t
  val cancel : t -> TYPES.invoke -> t
  val remember : t -> int -> history -> t
  val recall : t -> int -> history
  val finalize : t -> t
end
