module rec TYPES:
  sig
    type float64 = float
    type uint = int
    type float = TYPES.float64
    type binary = string
    type ref = TYPES.uint
    type binding =
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
    type document = Document.t
    type state = State.t
    type transition = Transition.t
    type invoke = Invoke.t
    type param = Param.t
    type assign = Assign.t
    type foreach = Foreach.t
    type case = Case.t
    type case_clause = Case_clause.t
    type log = Log.t
    type raise = Raise.t
    type var = Var.t
  end = TYPES
and Document:
  sig
    type t = {
      mutable name: string option;
      mutable binding: TYPES.binding;
      mutable datamodel: TYPES.param list;
      mutable initial_transitions: TYPES.transition list;
      mutable states: TYPES.state list;
    }
  end = Document
and State:
  sig
    type t = {
      mutable idx: TYPES.ref;
      mutable depth: TYPES.uint;
      mutable priority: TYPES.uint;
      mutable id: string option;
      mutable type_: TYPES.state_type;
      mutable initial_states: TYPES.ref list;
      mutable transitions: TYPES.transition list;
      mutable invocations: TYPES.invoke list;
      mutable on_enter: TYPES.expression list;
      mutable on_exit: TYPES.expression list;
      mutable children: TYPES.ref list;
      mutable parent: TYPES.ref option;
      mutable ancestors: TYPES.ref list;
      mutable descendants: TYPES.ref list;
      mutable history: TYPES.ref option;
      mutable history_type: TYPES.history_type option;
    }
  end = State
and Transition:
  sig
    type t = {
      mutable scope: TYPES.ref;
      mutable depth: TYPES.uint;
      mutable priority: TYPES.uint;
      mutable source: TYPES.ref option;
      mutable targets: TYPES.ref list;
      mutable events: string list;
      mutable condition: TYPES.expression option;
      mutable type_: TYPES.transition_type;
      mutable on_transition: TYPES.expression list;
    }
  end = Transition
and Invoke:
  sig
    type t = {
      mutable type_: TYPES.expression option;
      mutable src: TYPES.expression option;
      mutable id: TYPES.expression option;
      mutable namelist: TYPES.var list;
      mutable autoforward: bool;
      mutable params: TYPES.param list;
      mutable content: TYPES.content option;
      mutable on_exit: TYPES.expression list;
    }
  end = Invoke
and Param:
  sig
    type t = {
      mutable id: string;
      mutable expression: TYPES.expression option;
    }
  end = Param
and Assign:
  sig
    type t = {
      mutable id: string;
      mutable expression: TYPES.expression option;
    }
  end = Assign
and Foreach:
  sig
    type t = {
      mutable array: TYPES.expression;
      mutable item: TYPES.var option;
      mutable index: TYPES.var option;
      mutable expressions: TYPES.expression list;
    }
  end = Foreach
and Case:
  sig
    type t = {
      mutable clauses: TYPES.case_clause list;
    }
  end = Case
and Case_clause:
  sig
    type t = {
      mutable clause: TYPES.expression;
      mutable body: TYPES.expression;
    }
  end = Case_clause
and Log:
  sig
    type t = {
      mutable label: string option;
      mutable expression: TYPES.expression option;
    }
  end = Log
and Raise:
  sig
    type t = {
      mutable event: string;
    }
  end = Raise
and Var:
  sig
    type t = {
      mutable name: string;
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
