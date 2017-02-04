module rec TYPES:
  sig
    type float64 = float
    type uint = int
    type float = TYPES.float64
    type binary = string
    type ref = TYPES.uint

    type expression_type = [
      | `equal
      | `not_equal
      | `list
      | `map
      | `int
      | `float
      | `string
    ]
    type content =
      [
        | `string of string
        | `expression of TYPES.expression
        | `document of TYPES.document
      ]
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
    type expression = Expression.t
    type invoke = Invoke.t
    type param = Param.t
    type state = State.t
    type transition = Transition.t
  end = TYPES
and Document:
  sig
    type t = {
      name: string option;
      initial_transitions: TYPES.transition list;
      states: TYPES.state array;
    }
  end = Document
and Expression:
  sig
    type t = {
      t: TYPES.expression_type;
      bool_val: bool option;
      int_val: int option;
      float_val: float option;
      string_val: string option;
      args: t list;
    }
  end = Expression
and Invoke:
  sig
    type t = {
      type_: TYPES.expression option;
      src: TYPES.expression option;
      id: TYPES.expression option;
      namelist: TYPES.expression list;
      autoforward: bool;
      params: TYPES.param list;
      content: TYPES.content option;
      on_exit: TYPES.expression list;
    }
  end = Invoke
and Param:
  sig
    type t = {
      id: string;
      expression: TYPES.expression option;
    }
  end = Param
and State:
  sig
    type t = {
      idx: TYPES.ref;
      depth: TYPES.uint;
      id: string option;
      t: TYPES.state_type;
      initial: TYPES.ref list;
      transitions: TYPES.transition list;
      invocations: TYPES.invoke list;
      on_enter: TYPES.expression list;
      on_exit: TYPES.expression list;
      children: TYPES.ref list;
      parent: TYPES.ref option;
      ancestors: TYPES.ref list;
      descendants: TYPES.ref list;
      history: TYPES.ref option;
      history_type: TYPES.history_type option;
    }
  end = State
and Transition:
  sig
    type t = {
      scope: TYPES.ref;
      depth: TYPES.uint;
      priority: TYPES.uint;
      source: TYPES.ref option;
      targets: TYPES.ref list;
      events: string list;
      condition: TYPES.expression option;
      type_: TYPES.transition_type;
      on_transition: TYPES.expression list;
    }
  end = Transition

include TYPES
