module rec TYPES:
  sig
    type float64 = float
    type uint = int
    type float = TYPES.float64
    type binary = string
    type ref = TYPES.uint
    type expression =
      [
        | `bool of bool
        | `int of int
        | `float of TYPES.float
        | `string of string
        | `binary of TYPES.binary
        | `assign of TYPES.assign
        | `foreach of TYPES.foreach
        | `case of TYPES.case
        | `log of TYPES.log
        | `raise of TYPES.raise
        | `var of TYPES.var
      ]
    type content =
      [
        | `string of string
        | expression
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
    type assign = Assign.t
    type case = Case.t
    type case_clause = Case_clause.t
    type document = Document.t
    type foreach = Foreach.t
    type invoke = Invoke.t
    type log = Log.t
    type param = Param.t
    type raise = Raise.t
    type state = State.t
    type transition = Transition.t
    type var = Var.t
  end = TYPES
and Assign:
  sig
    type t = {
      id: string;
      expression: TYPES.expression option;
    }
  end = Assign
and Case:
  sig
    type t = {
      clauses: TYPES.case_clause list;
    }
  end = Case
and Case_clause:
  sig
    type t = {
      clause: TYPES.expression;
      body: TYPES.expression;
    }
  end = Case_clause
and Document:
  sig
    type t = {
      name: string option;
      binding: TYPES.document_binding;
      initial_transition: TYPES.transition;
      states: TYPES.state array;
      datamodel: TYPES.param list;
    }
  end = Document
and Foreach:
  sig
    type t = {
      array: TYPES.expression;
      item: TYPES.var option;
      index: TYPES.var option;
      expressions: TYPES.expression list;
    }
  end = Foreach
and Invoke:
  sig
    type t = {
      type_: TYPES.expression option;
      src: TYPES.expression option;
      id: TYPES.expression option;
      namelist: TYPES.var list;
      autoforward: bool;
      params: TYPES.param list;
      content: TYPES.content option;
      on_exit: TYPES.expression list;
    }
  end = Invoke
and Log:
  sig
    type t = {
      label: string option;
      expression: TYPES.expression option;
    }
  end = Log
and Param:
  sig
    type t = {
      id: string;
      expression: TYPES.expression option;
    }
  end = Param
and Raise:
  sig
    type t = {
      event: string;
    }
  end = Raise
and State:
  sig
    type t = {
      idx: TYPES.ref;
      depth: TYPES.uint;
      priority: TYPES.uint;
      id: string option;
      type_: TYPES.state_type;
      initial_states: TYPES.ref list;
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
and Var:
  sig
    type t = {
      name: string;
    }
  end = Var

include TYPES

let compare_state s1 s2 =
  match s1.State.depth, s2.State.depth with
  | d1, d2 when d1 = d2 -> Pervasives.compare s1.State.priority s2.State.priority
  | d1, d2 when d1 >= d2 -> 1
  | _ -> -1

let compare_transition t1 t2 =
  match t1.Transition.depth, t2.Transition.depth with
  | d1, d2 when d1 = d2 -> Pervasives.compare t1.Transition.priority t2.Transition.priority
  | d1, d2 when d1 >= d2 -> 1
  | _ -> -1

let compare_transition_reverse t1 t2 =
  match t2.Transition.depth, t1.Transition.depth with
  | d1, d2 when d1 = d2 -> Pervasives.compare t2.Transition.priority t1.Transition.priority
  | d1, d2 when d1 >= d2 -> -1
  | _ -> 1
