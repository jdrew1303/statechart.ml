module rec TYPES:
  sig
    type executable =
      [ `raise of TYPES.raise
      | `case of TYPES.case
      | `case_clause of TYPES.case_clause
      | `foreach of TYPES.foreach
      | `log of TYPES.log
        (* Data Model *)
      | `data_model of TYPES.data_model
      | `data of TYPES.data
      | `assign of TYPES.assign
      | `done_data of TYPES.done_data
      | `content of TYPES.content
      | `param of TYPES.param
      | `script of TYPES.script
        (* External *)
      | `send of TYPES.send
      | `cancel of TYPES.cancel
      | `invoke of TYPES.invoke
      | `finalize of TYPES.finalize
      ]
    type expr = Expr of string | ExprValue of string
    type document_binding =
      [
        | `early
        | `late
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
    (* Core *)
    type document = Document.t
    type state = State.t
    type parallel = Parallel.t
    type transition = Transition.t
    type initial = Initial.t
    type final = Final.t
    type on_entry = OnEntry.t
    type on_exit = OnExit.t
    type history = History.t
    (* Executable *)
    type raise = Raise.t
    type case = Case.t
    type case_clause = CaseClause.t
    type foreach = Foreach.t
    type log = Log.t
    (* Data Model *)
    type data_model = DataModel.t
    type data = Data.t
    type assign = Assign.t
    type done_data = DoneData.t
    type content = Content.t
    type param = Param.t
    type script = Script.t
    (* External *)
    type send = Send.t
    type cancel = Cancel.t
    type invoke = Invoke.t
    type finalize = Finalize.t
  end = TYPES
and Document:
  sig
    type child = [ `state of TYPES.state
                 | `parallel of TYPES.parallel
                 | `final of TYPES.final
                 | `data_model of TYPES.data_model
                 | `script of TYPES.script
                 ]
    type t = {
      name: string option;
      data_model: string option;
      binding: TYPES.document_binding;
      children: child list;
    }
  end = Document
and State:
  sig
    type child = [ `on_entry of TYPES.on_entry
                 | `on_exit of TYPES.on_exit
                 | `transition of TYPES.transition
                 | `initial of TYPES.initial
                 | `state of TYPES.state
                 | `parallel of TYPES.parallel
                 | `final of TYPES.final
                 | `history of TYPES.history
                 | `data_model of TYPES.data_model
                 | `invoke of TYPES.invoke
                 ]
    type t = {
      id: string option;
      initial: string list;
      children: child list;
    }
  end = State
and Parallel:
  sig
    type child = [ `on_entry of TYPES.on_entry
                 | `on_exit of TYPES.on_exit
                 | `transition of TYPES.transition
                 | `state of TYPES.state
                 | `parallel of TYPES.parallel
                 | `history of TYPES.history
                 | `data_model of TYPES.data_model
                 | `invoke of TYPES.invoke
                 ]
    type t = {
      id: string option;
      children: child list;
    }
  end = Parallel
and Transition:
  sig
    type child = TYPES.executable
    type t = {
      event: string list;
      cond: string;
      target: string list;
      t: TYPES.transition_type;
      children: child list;
    }
  end = Transition
and Initial:
  sig
    type child = TYPES.transition
    type t = {
      children: child list;
    }
  end = Initial
and Final:
  sig
    type child = [ `on_entry of TYPES.on_entry
                 | `on_exit of TYPES.on_exit
                 | `done_data of TYPES.done_data
                 ]
    type t = {
      id: string option;
      children: child list;
    }
  end = Final
and OnEntry:
  sig
    type child = TYPES.executable
    type t = {
      children: child list;
    }
  end = OnEntry
and OnExit:
  sig
    type child = TYPES.executable
    type t = {
      children: child list;
    }
  end = OnExit
and History:
  sig
    type child = TYPES.transition
    type t = {
      id: string option;
      children: child list;
      t: TYPES.history_type;
    }
  end = History
and Raise:
  sig
    type t = {
      event: string option;
    }
  end = Raise
and Case:
  sig
    type t = {
      children: TYPES.case_clause
    }
  end = Case
and CaseClause:
  sig
    type t = {
      cond: string;
      children: TYPES.executable list;
    }
  end = CaseClause
and Foreach:
  sig
    type t = {
      array: string;
      item: string option;
      index: string option;
      children: TYPES.executable list;
    }
  end = Foreach
and Log:
  sig
    type t = {
      label: string option;
      expr: string option;
    }
  end = Log
and DataModel:
  sig
    type t = {
      children: TYPES.data list;
    }
  end = DataModel
and Data:
  sig
    type t = {
      id: string;
      src: string option;
      expr: string option;
      children: string option; (* TODO make this a more complex type *)
    }
  end = Data
and Assign:
  sig
    type t = {
      location: string;
      expr: string option;
      children: string option; (* TODO make this a more complex type *)
    }
  end = Assign
and DoneData:
  sig
    type child = [ `content of TYPES.content
                 | `param of TYPES.param
                 ]
    type t = {
      children: child option;
    }
  end = DoneData
and Content:
  sig
    type t = {
      expr: string option;
      children: string option; (* TODO make this a more complex type *)
    }
  end = Content
and Param:
  sig
    type t = {
      name: string;
      expr: string option;
      location: string option;
    }
  end = Param
and Script:
  sig
    type t = {
      src: string option;
      children: string option;
    }
  end = Script
and Send:
  sig
    type child = [ `param of TYPES.param
                 | `content of TYPES.content
                 ]
    type t = {
      event: TYPES.expr;
      target: TYPES.expr option;
      t: TYPES.expr;
      id: TYPES.expr option;
      delay: TYPES.expr option;
      namelist: string list option;
      children: child list;
    }
  end = Send
and Cancel:
  sig
    type t = {
      sendid: TYPES.expr;
    }
  end = Cancel
and Invoke:
  sig
    type child = [ `param of TYPES.param
                 | `finalize of TYPES.finalize
                 | `content of TYPES.content
                 ]
    type t = {
      t: TYPES.expr option;
      src: TYPES.expr option;
      id: TYPES.expr option;
      namelist: string list option;
      autoforward: bool option;
      children: child list option;
    }
  end = Invoke
and Finalize:
  sig
    type t = {
      children: TYPES.executable list;
    }
  end = Finalize

include TYPES
