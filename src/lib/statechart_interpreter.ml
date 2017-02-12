module Bitset = Statechart_bitset

module type Engine = sig
  type datamodel
  type executable

  val load_executable : Statechart_executable.expression -> executable
  val load_query : Statechart_executable.expression -> (datamodel -> bool)
  val load_event_match : string array -> (string -> bool)
end

module type Interpreter = sig
  type t
  type datamodel
  type document
  type invoke
  type executable
  type event

  val load : Statechart_executable.document -> document

  val start : document -> datamodel -> t
  val handle_event : document -> t -> event -> t
  val synchronize : document -> t -> t
  val invoke : document -> t -> t
  val stop : document -> t -> t

  val get_configuration : t -> int array
  val get_configuration_names : t -> document -> string array
  val get_datamodel : t -> datamodel
  val put_datamodel : t -> datamodel -> t
  val get_executions : t -> executable array
end

module Make(Eng : Engine) = struct
  type datamodel = Eng.datamodel
  type executable = Eng.executable
  type query = datamodel -> bool
  type event = string

  module rec TYPES:
    sig
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
          | `targetless
          | `internal
          | `spontaneous
          | `history
          | `initial
          | `external_
        ]
      type document = Document.t
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
  and Invoke:
    sig
      type t = {
        t: executable option;
        src: executable option;
        id: executable option;
        autoforward: bool;
        params: TYPES.param array;
        content: executable option;
        on_exit: executable array;
      }
    end = Invoke
  and Param:
    sig
      type t = {
        id: string;
        expression: executable option;
      }
    end = Param
  and State:
    sig
      type t = {
        t: TYPES.state_type;
        idx: int;
        id: string option;
        on_enter: executable array;
        on_exit: executable array;
        invocations: TYPES.invoke array;
        data: executable array;
        donedata: executable option;
        parent: int;
        children: Bitset.t;
        ancestors: Bitset.t;
        completion: Bitset.t;
        transitions: int array;
      }
    end = State
  and Transition:
    sig
      type t = {
        t: TYPES.transition_type;
        idx: int;
        source: int;
        events: (string -> bool) option;
        condition: query option;
        on_transition: executable array;
        targets: Bitset.t;
        conflicts: Bitset.t;
        exits: Bitset.t;
      }
    end = Transition

  type document = TYPES.document
  type invoke = TYPES.invoke
  type param = TYPES.param
  type state = TYPES.state
  type transition = TYPES.transition

  type t = {
    configuration: Bitset.t;
    history: Bitset.t;
    invocations: Bitset.t;
    initialized: Bitset.t;
    datamodel: datamodel;
    executions: executable array;
  }

  let resolve doc idx = Array.get doc.Document.states idx
  let resolve_transition doc idx = Array.get doc.Document.transitions idx

  let add_entry_ancestors doc entry_set =
    Bitset.iter_left (fun idx ->
      let {State.ancestors} = resolve doc idx in
      Bitset.bor entry_set ancestors
    ) entry_set

  let add_entry_descendants_history doc entry_set trans_set state =
    (* TODO *)
    ()

  let add_entry_descendants_initial doc entry_set trans_set state =
    Array.iter (fun idx ->
      let transition = resolve_transition doc idx in
      let targets = transition.Transition.targets in
      Bitset.bor entry_set targets;
      Bitset.clear entry_set state.State.idx;
      Bitset.set trans_set idx;
      Bitset.iter_left (fun idx ->
        let state = resolve doc idx in
        Bitset.bor entry_set state.State.ancestors
      ) targets;
    ) state.State.transitions

  let should_add_compound_state configuration entry_set exit_set state =
    let {State.children} = state in
    (not (Bitset.has_and entry_set children)) && (
      (not (Bitset.has_and configuration children)) ||
      (Bitset.has_and exit_set children)
    )

  let add_entry_descendants_compound doc configuration entry_set trans_set exit_set state =
    if should_add_compound_state configuration entry_set exit_set state
    then (
      let completion = state.State.completion in
      Bitset.bor entry_set completion;
      if not (Bitset.has_and completion state.State.children)
      then match Bitset.first completion with
      | None -> ()
      | Some idx -> (
        let child = resolve doc idx in
        Bitset.bor entry_set child.State.ancestors
      )
    )

  let add_entry_descendants doc engine entry_set exit_set trans_set =
    let configuration = engine.configuration in
    Bitset.iter_left (fun idx ->
      let state = resolve doc idx in
      match state.State.t with
      | `parallel ->
        Bitset.bor entry_set state.State.completion
      | t when t == `history_shallow || t == `history_deep ->
        add_entry_descendants_history doc entry_set trans_set state
      | `initial ->
        add_entry_descendants_initial doc entry_set trans_set state
      | `compound ->
        add_entry_descendants_compound doc configuration entry_set trans_set exit_set state
      | _ ->
        ()
    ) entry_set

  let exit_states doc engine exit_set =
    let executions = Bitset.fold_right (fun execs idx ->
      let state = resolve doc idx in
      Array.append execs state.State.on_exit
    ) engine.executions exit_set in
    {engine with executions}

  let take_transitions doc engine trans_set =
    let executions = Bitset.fold_left (fun execs idx ->
      let transition = resolve_transition doc idx in
      Array.append execs transition.Transition.on_transition
    ) engine.executions trans_set in
    {engine with executions}

  let initialize_data engine state =
    let idx = state.State.idx in
    let initialized = engine.initialized in
    if Bitset.get initialized idx
    then engine
    else (
      Bitset.set initialized idx;
      let executions = Array.append engine.executions state.State.data in
      {engine with executions}
    )

  let enter_state doc engine state =
    let engine = initialize_data engine state in
    let {executions} = engine in
    let executions = Array.append executions state.State.on_enter in
    (* TODO take history and initial transitions *)
    (* TODO handle final states *)
    {engine with executions}

  let is_proper_state state =
    let t = state.State.t in
    t != `history_shallow && t != `history_deep && t != `initial

  let enter_states doc engine entry_set =
    let initialized = Bitset.copy engine.initialized in
    let engine = {engine with initialized} in
    Bitset.fold_left (fun engine idx ->
      let configuration = engine.configuration in
      let state = resolve doc idx in
      if Bitset.get configuration idx && is_proper_state state
      then engine
      else enter_state doc engine state
    ) engine entry_set

  let establish_entryset doc engine entry_set trans_set exit_set =
    add_entry_ancestors doc entry_set;
    add_entry_descendants doc engine entry_set exit_set trans_set;
    let engine = exit_states doc engine exit_set in
    let engine = take_transitions doc engine trans_set in
    enter_states doc engine entry_set

  let is_transition_conflict_free transition conflicts =
    not (Bitset.get conflicts transition.Transition.idx)

  let is_transition_active transition configuration =
    Bitset.get configuration transition.Transition.source

  let is_transition_applicable transition event =
    match transition.Transition.events, event with
    | None, None -> true
    | Some matcher, Some evt -> matcher evt
    | _ -> false

  let is_transition_enabled transition datamodel =
    match transition.Transition.condition with
    | None -> true
    | Some cond -> cond datamodel

  let select_active_transitions doc engine event =
    let transitions = doc.Document.transitions in
    let configuration = engine.configuration in
    let datamodel = engine.datamodel in
    let target_set = Bitset.copy_clear configuration in
    let trans_set = Bitset.make (Array.length transitions) in
    let exit_set = Bitset.copy_clear configuration in
    let conflicts = Bitset.copy_clear configuration in
    Array.iter (fun transition ->
      match transition with
      (* never select history or initial transitions automatically *)
      | {Transition.t} when t = `history || t == `initial ->
        ()
      | _ ->
        if (is_transition_active transition configuration)
        && (is_transition_conflict_free transition conflicts)
        && (is_transition_applicable transition event)
        && (is_transition_enabled transition datamodel)
        then (
          Bitset.bor target_set transition.Transition.targets;
          Bitset.set trans_set transition.Transition.idx;
          Bitset.bor exit_set transition.Transition.exits;
          Bitset.bor conflicts transition.Transition.conflicts
        )
    ) transitions;
    target_set, trans_set, exit_set

  let remember_history doc engine exit_set =
    let configuration = engine.configuration in
    let history = Bitset.copy engine.history in
    Array.iter (fun state ->
      match state with
      | {State.t; parent; completion} when t == `history_deep || t == `history_shallow -> (
        if Bitset.get exit_set parent
        then (
          let tmp_states = Bitset.copy completion in
          Bitset.band tmp_states configuration;
          Bitset.bxor history completion;
          Bitset.bor history tmp_states;
        )
      )
      | _ -> ()
    ) doc.Document.states;
    {engine with history}

  let select_transitions doc engine event =
    let engine = {engine with executions=[||]} in
    let target_set, trans_set, exit_set = select_active_transitions doc engine event in
    if Bitset.has_any trans_set
    (* We are done with the internal events *)
    then engine
    else (
      let engine = remember_history doc engine exit_set in
      establish_entryset doc engine target_set trans_set exit_set
    )

  let load_array l =
    Array.map Eng.load_executable l

  let load_option l =
    match l with
    | None -> None
    | Some expr -> Some (Eng.load_executable expr)

  let load_transition transition =
    {
      Transition.idx=transition.Statechart_executable.Transition.idx;
      source=transition.Statechart_executable.Transition.source;
      targets=transition.Statechart_executable.Transition.targets;
      events=(match transition.Statechart_executable.Transition.events with
      | [||] -> None
      | events -> Some (Eng.load_event_match events));
      condition=(match transition.Statechart_executable.Transition.condition with
      | None -> None
      | Some c -> Some (Eng.load_query c));
      t=transition.Statechart_executable.Transition.t;
      on_transition=load_array transition.Statechart_executable.Transition.on_transition;
      conflicts=transition.Statechart_executable.Transition.conflicts;
      exits=transition.Statechart_executable.Transition.exits;
    }

  let load_param p =
    {
      Param.id=p.Statechart_executable.Param.id;
      expression=load_option p.Statechart_executable.Param.expression;
    }

  let load_invoke inv =
    {
      Invoke.t=load_option inv.Statechart_executable.Invoke.t;
      src=load_option inv.Statechart_executable.Invoke.src;
      id=load_option inv.Statechart_executable.Invoke.id;
      autoforward=inv.Statechart_executable.Invoke.autoforward;
      params=Array.map load_param inv.Statechart_executable.Invoke.params;
      (* TODO *)
      content=None;
      on_exit=load_array inv.Statechart_executable.Invoke.on_exit;
    }

  let load_state state =
    {
      State.idx=state.Statechart_executable.State.idx;
      id=state.Statechart_executable.State.id;
      t=state.Statechart_executable.State.t;
      transitions=state.Statechart_executable.State.transitions;
      invocations=Array.map load_invoke state.Statechart_executable.State.invocations;
      on_enter=load_array state.Statechart_executable.State.on_enter;
      on_exit=load_array state.Statechart_executable.State.on_exit;
      children=state.Statechart_executable.State.children;
      parent=state.Statechart_executable.State.parent;
      ancestors=state.Statechart_executable.State.ancestors;
      completion=state.Statechart_executable.State.completion;
      data=load_array state.Statechart_executable.State.data;
      donedata=load_option state.Statechart_executable.State.donedata;
    }

  (* Public interface *)

  let load src =
    {
      Document.name=src.Statechart_executable.Document.name;
      transitions=Array.map load_transition src.Statechart_executable.Document.transitions;
      states=Array.map load_state src.Statechart_executable.Document.states;
    }

  let start doc datamodel =
    let states = doc.Document.states in
    let empty = Bitset.make (Array.length states) in
    let engine = {
      configuration=empty;
      history=empty;
      initialized=empty;
      invocations=empty;
      executions=[||];
      datamodel=datamodel;
    } in
    let initial_state = Array.get states 0 in
    let target_set = initial_state.State.completion in
    let trans_set = empty in
    let exit_set = empty in
    establish_entryset doc engine target_set trans_set exit_set

  let handle_event doc engine event =
    select_transitions doc engine (Some event)

  let synchronize doc engine =
    select_transitions doc engine None

  let invoke doc engine =
    (* TODO un/invoke all of the enabled states *)
    engine

  let stop doc engine =
    let states = doc.Document.states in
    let executions = Array.fold_right (fun state acc ->
      if Bitset.get engine.configuration state.State.idx
      then Array.append acc state.State.on_exit
      else acc
    ) states engine.executions in
    {engine with executions; invocations=Bitset.make (Array.length states)}

  let get_configuration engine = Bitset.to_idx_array engine.configuration
  let get_configuration_names engine doc =
    let configuration = engine.configuration in
    Bitset.fold_left (fun acc idx ->
      match resolve doc idx with
      | {State.id=Some id} -> Array.append acc [| id |]
      | _ -> acc
    ) [| |] configuration

  let get_datamodel engine = engine.datamodel
  let put_datamodel engine datamodel = {engine with datamodel=datamodel}
  let get_executions engine = engine.executions
end
