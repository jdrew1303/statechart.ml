module rec TYPES:
  sig
    type float64 = float
    type uint = int
    type float = TYPES.float64
    type binary = string
    type bitset = Statechart_bitset.t
    type content =
      [
        | `string of string
        | `expression of TYPES.expression
        | `document of TYPES.document
      ]
    type expression_type =
      [
        | `block
        | `null
        | `list
        | `map
        | `bool
        | `int
        | `float
        | `string
        | `raise
        | `case
        | `clause
        | `foreach
        | `log
        | `assign
        | `send
        | `cancel
        | `equal
        | `not_equal
      ]
    type state_type =
      [
        | `compound
        | `atomic
        | `parallel
        | `history_shallow
        | `history_deep
        | `initial
        | `final
      ]
    type transition_type =
      [
        | `external_
        | `targetless
        | `internal
        | `spontaneous
        | `history
        | `initial
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
      states: TYPES.state array;
      transitions: TYPES.transition array;
    }
  end = Document
and Expression:
  sig
    type t = {
      t: TYPES.expression_type;
      bool_val: bool option;
      int_val: int option;
      float_val: TYPES.float option;
      string_val: string option;
      args: TYPES.expression list;
    }
  end = Expression
and Invoke:
  sig
    type t = {
      t: TYPES.expression option;
      src: TYPES.expression option;
      id: TYPES.expression option;
      autoforward: bool;
      params: TYPES.param array;
      content: TYPES.content option;
      on_exit: TYPES.expression array;
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
      t: TYPES.state_type;
      idx: TYPES.uint;
      id: string option;
      on_enter: TYPES.expression array;
      on_exit: TYPES.expression array;
      invocations: TYPES.invoke array;
      data: TYPES.expression array;
      donedata: TYPES.expression option;
      parent: TYPES.uint;
      children: TYPES.bitset;
      ancestors: TYPES.bitset;
      completion: TYPES.bitset;
      transitions: TYPES.bitset;
      has_history: bool;
    }
  end = State
and Transition:
  sig
    type t = {
      t: TYPES.transition_type;
      idx: TYPES.uint;
      source: TYPES.uint;
      events: string array;
      condition: TYPES.expression option;
      on_transition: TYPES.expression array;
      targets: TYPES.bitset;
      conflicts: TYPES.bitset;
      exits: TYPES.bitset;
    }
  end = Transition

include TYPES

let bool_expr v =
  {
    Expression.t=`bool;
    bool_val=Some v;
    int_val=None;
    float_val=None;
    string_val=None;
    args=[];
  }

let int_expr v =
  {
    Expression.t=`int;
    bool_val=None;
    int_val=Some v;
    float_val=None;
    string_val=None;
    args=[];
  }

let float_expr v =
  {
    Expression.t=`float;
    bool_val=None;
    int_val=None;
    float_val=Some v;
    string_val=None;
    args=[];
  }

let string_expr v =
  {
    Expression.t=`string;
    bool_val=None;
    int_val=None;
    float_val=None;
    string_val=Some v;
    args=[];
  }

let complex_expr t args =
  {
    Expression.t=t;
    bool_val=None;
    int_val=None;
    float_val=None;
    string_val=None;
    args=args;
  }

let null = complex_expr `null []
