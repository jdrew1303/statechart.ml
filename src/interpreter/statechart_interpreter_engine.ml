module IntSet = Set.Make(struct
  type t = int
  let compare = compare
end)

module type Engine = sig
  type t
  type executable
  type ref = int
  type configuration = IntSet.t
  type event = string
  type history = int list

  module rec TYPES:
    sig
      type document_binding =
        [
          | `early
          | `late
        ]
      type state_type =
        [
          | `composite
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
        type_: executable option;
        src: executable option;
        id: executable option;
        namelist: executable list;
        autoforward: bool;
        params: TYPES.param list;
        content: executable option;
        on_exit: executable list;
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
        idx: ref;
        depth: int;
        order: int;
        id: string option;
        type_: TYPES.state_type;
        initial_state: ref option;
        transitions: TYPES.transition list;
        invocations: TYPES.invoke list;
        on_enter: executable list;
        on_exit: executable list;
        children: IntSet.t;
        parent: ref option;
        ancestors: IntSet.t;
        descendants: IntSet.t;
        history: ref list;
        history_type: TYPES.history_type option;
      }
    end
  and Transition:
    sig
      type t = {
        domain: ref;
        depth: int;
        order: int;
        source: ref option;
        targets: ref list;
        events: string list;
        condition: executable option;
        type_: TYPES.transition_type;
        on_transition: executable list;
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

  val match_event : string list -> string -> bool
  val query : t -> executable -> bool
  val execute : t -> executable -> t
  val send : t -> event -> t
  val invoke : t -> TYPES.invoke -> t
  val cancel : t -> TYPES.invoke -> t
  val remember : t -> int -> history -> t
  val recall : t -> int -> history
end
