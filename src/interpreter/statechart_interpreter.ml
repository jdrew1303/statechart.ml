open Statechart_interpreter_engine

module IntSet = Set.Make(struct
  type t = int
  let compare = compare
end)
module IntSetRev = Set.Make(struct
  type t = int
  let compare a b =
    compare b a
end)

module type Interpreter = sig
  type t
  type datamodel
  type document
  type invoke
  type executable
  type event

  val load : Statechart.document -> document

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
        children: IntSet.t;
        ancestors: IntSet.t;
        completion: IntSet.t;
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
        targets: IntSet.t;
        conflicts: IntSet.t;
        exits: IntSetRev.t;
      }
    end = Transition

  type document = TYPES.document
  type invoke = TYPES.invoke
  type param = TYPES.param
  type state = TYPES.state
  type transition = TYPES.transition

  type t = {
    configuration: IntSet.t;
    history: IntSet.t;
    invocations: IntSet.t;
    initialized: IntSet.t;
    datamodel: datamodel;
    executions: executable array;
  }

  let resolve doc idx = Array.get doc.Document.states idx
  let resolve_transition doc idx = Array.get doc.Document.transitions idx

  let has_intersection a b =
    (IntSet.subset a b) || (IntSet.subset b a)

  let cast_exit_set a =
    IntSetRev.fold IntSet.add a IntSet.empty

  let add_entry_ancestors doc entry_set =
    IntSet.fold (fun idx acc ->
      let {State.ancestors} = resolve doc idx in
      IntSet.union acc ancestors
    ) entry_set entry_set

  let add_entry_descendants_history doc acc state =
    (* TODO *)
    acc

  let add_entry_descendants_initial doc acc state =
    Array.fold_left (fun acc idx ->
      let entry_set, trans_set = acc in
      let transition = resolve_transition doc idx in
      let targets = transition.Transition.targets in
      let entry_set = IntSet.remove state.State.idx entry_set in
      let entry_set = IntSet.union entry_set targets in
      let trans_set = IntSet.add idx trans_set in
      let entry_set = IntSet.fold (fun idx entry_set ->
        let state = resolve doc idx in
        IntSet.union entry_set state.State.ancestors
      ) targets entry_set in
      entry_set, trans_set
    ) acc state.State.transitions

  let should_add_compound_state configuration entry_set exit_set state =
    let {State.children} = state in
    (not (has_intersection entry_set children)) && (
      (not (has_intersection configuration children)) ||
      (has_intersection exit_set children)
    )

  let add_entry_descendants_compound doc configuration acc exit_set state =
    let entry_set, trans_set = acc in
    if should_add_compound_state configuration entry_set exit_set state
    then (
      let completion = state.State.completion in
      let entry_set = IntSet.union entry_set completion in
      if (not (has_intersection completion state.State.children))
      then (
        try
          let idx = IntSet.choose completion in
          let child = resolve doc idx in
          let entry_set = IntSet.union entry_set child.State.ancestors in
          entry_set, trans_set
        with
        | _ -> entry_set, trans_set
      )
      else entry_set, trans_set
    )
    else entry_set, trans_set

  let add_entry_descendants doc engine entry_set exit_set trans_set =
    let configuration = engine.configuration in
    let exit_set = cast_exit_set exit_set in
    IntSet.fold (fun idx acc ->
      let state = resolve doc idx in
      match state.State.t with
      | `parallel ->
        let entry_set, trans_set = acc in
        let entry_set = IntSet.union entry_set state.State.completion in
        entry_set, trans_set
      | t when t == `history_shallow || t == `history_deep ->
        add_entry_descendants_history doc acc state
      | `initial ->
        add_entry_descendants_initial doc acc state
      | `compound ->
        add_entry_descendants_compound doc configuration acc exit_set state
      | _ ->
        acc
    ) entry_set (entry_set, trans_set)

  let exit_states doc engine exit_set =
    let executions = IntSetRev.fold (fun idx execs ->
      let state = resolve doc idx in
      Array.append execs state.State.on_exit
    ) exit_set engine.executions in
    {engine with executions}

  let take_transitions doc engine trans_set =
    let executions = IntSet.fold (fun idx execs ->
      let transition = resolve_transition doc idx in
      Array.append execs transition.Transition.on_transition
    ) trans_set engine.executions in
    {engine with executions}

  let initialize_data engine state =
    let idx = state.State.idx in
    let initialized = engine.initialized in
    if IntSet.mem idx initialized
    then engine
    else (
      let initialized = IntSet.add idx initialized in
      let executions = Array.append engine.executions state.State.data in
      {engine with initialized; executions}
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
    IntSet.fold (fun idx engine ->
      let configuration = engine.configuration in
      let state = resolve doc idx in
      if IntSet.mem idx configuration && is_proper_state state
      then engine
      else enter_state doc engine state
    ) entry_set engine

  let establish_entryset doc engine entry_set trans_set exit_set =
    let entry_set = add_entry_ancestors doc entry_set in
    let entry_set, trans_set = add_entry_descendants doc engine entry_set exit_set trans_set in
    let engine = exit_states doc engine exit_set in
    let engine = take_transitions doc engine trans_set in
    enter_states doc engine entry_set

  let is_transition_conflict_free transition conflicts =
    not (IntSet.mem transition.Transition.idx conflicts)

  let is_transition_active transition configuration =
    IntSet.mem transition.Transition.source configuration

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
    let configuration = engine.configuration in
    let datamodel = engine.datamodel in
    let target_set = IntSet.empty in
    let trans_set = IntSet.empty in
    let exit_set = IntSetRev.empty in
    let conflicts = IntSet.empty in
    let init = target_set, trans_set, exit_set, conflicts in
    let target_set, trans_set, exit_set, _conflicts = Array.fold_left (fun acc transition ->
      match transition with
      (* never select history or initial transitions automatically *)
      | {Transition.t} when t = `history || t == `initial ->
        acc
      | _ ->
        let target_set, trans_set, exit_set, conflicts = acc in
        if (is_transition_active transition configuration)
        && (is_transition_conflict_free transition conflicts)
        && (is_transition_applicable transition event)
        && (is_transition_enabled transition datamodel)
        then (
          let target_set = IntSet.union target_set transition.Transition.targets in
          let trans_set = IntSet.add transition.Transition.idx trans_set in
          let exit_set = IntSetRev.union exit_set transition.Transition.exits in
          let conflicts = IntSet.union conflicts transition.Transition.conflicts in
          target_set, trans_set, exit_set, conflicts
        )
        else acc
    ) init doc.Document.transitions in
    target_set, trans_set, exit_set

  let remember_history doc engine exit_set =
    let configuration = engine.configuration in
    let history = Array.fold_left (fun acc state ->
      match state with
      | {State.t; parent; completion} when t == `history_deep || t == `history_shallow -> (
        if IntSetRev.mem parent exit_set
        then (
          let tmp_states = IntSet.inter configuration completion in
          let history = IntSet.diff acc completion in
          IntSet.union history tmp_states
        )
        else acc
      )
      | _ -> acc
    ) engine.history doc.Document.states in
    {engine with history}

  let select_transitions doc engine event =
    let engine = {engine with executions=[||]} in
    let target_set, trans_set, exit_set = select_active_transitions doc engine event in
    if IntSet.is_empty trans_set
    (* We are done with the internal events *)
    then engine
    else (
      let engine = remember_history doc engine exit_set in
      establish_entryset doc engine target_set trans_set exit_set
    )

  let load_list l =
    Array.of_list (List.map Eng.load_executable l)

  let load_option l =
    match l with
    | None -> None
    | Some expr -> Some (Eng.load_executable expr)

  let load_transition transition =
    {
      Transition.idx=transition.Statechart.Transition.idx;
      source=transition.Statechart.Transition.source;
      targets=IntSet.of_list transition.Statechart.Transition.targets;
      events=(match transition.Statechart.Transition.events with
      | [] -> None
      | events -> Some (Eng.load_event_match events));
      condition=(match transition.Statechart.Transition.condition with
      | None -> None
      | Some c -> Some (Eng.load_query c));
      t=transition.Statechart.Transition.t;
      on_transition=load_list transition.Statechart.Transition.on_transition;
      conflicts=IntSet.of_list transition.Statechart.Transition.conflicts;
      exits=IntSetRev.of_list transition.Statechart.Transition.exits;
    }

  let load_param p =
    {
      Param.id=p.Statechart.Param.id;
      expression=load_option p.Statechart.Param.expression;
    }

  let load_invoke inv =
    {
      Invoke.t=load_option inv.Statechart.Invoke.t;
      src=load_option inv.Statechart.Invoke.src;
      id=load_option inv.Statechart.Invoke.id;
      autoforward=inv.Statechart.Invoke.autoforward;
      params=Array.of_list
        (List.map load_param inv.Statechart.Invoke.params);
      (* TODO *)
      content=None;
      on_exit=load_list inv.Statechart.Invoke.on_exit;
    }

  let load_state state =
    {
      State.idx=state.Statechart.State.idx;
      id=state.Statechart.State.id;
      t=state.Statechart.State.t;
      transitions=Array.of_list state.Statechart.State.transitions;
      invocations=Array.of_list
        (List.map load_invoke state.Statechart.State.invocations);
      on_enter=load_list state.Statechart.State.on_enter;
      on_exit=load_list state.Statechart.State.on_exit;
      children=IntSet.of_list state.Statechart.State.children;
      parent=state.Statechart.State.parent;
      ancestors=IntSet.of_list state.Statechart.State.ancestors;
      completion=IntSet.empty;
      data=load_list state.Statechart.State.data;
      donedata=load_option state.Statechart.State.donedata;
    }

  (* Public interface *)

  let load src =
    {
      Document.name=src.Statechart.Document.name;
      transitions=Array.of_list
        (List.map load_transition src.Statechart.Document.transitions);
      states=Array.of_list
        (List.map load_state src.Statechart.Document.states);
    }

  let start doc datamodel =
    let engine = {
      configuration=IntSet.empty;
      history=IntSet.empty;
      initialized=IntSet.empty;
      invocations=IntSet.empty;
      executions=[||];
      datamodel=datamodel;
    } in
    let initial_state = Array.get doc.Document.states 0 in
    let target_set = initial_state.State.completion in
    let trans_set = IntSet.empty in
    let exit_set = IntSetRev.empty in
    establish_entryset doc engine target_set trans_set exit_set

  let handle_event doc engine event =
    select_transitions doc engine (Some event)

  let synchronize doc engine =
    select_transitions doc engine None

  let invoke doc engine =
    (* TODO un/invoke all of the enabled states *)
    engine

  let stop doc engine =
    let executions = Array.fold_right (fun state acc ->
      if IntSet.mem state.State.idx engine.configuration
      then Array.append acc state.State.on_exit
      else acc
    ) doc.Document.states engine.executions in
    {engine with executions; invocations=IntSet.empty}

  let get_configuration engine = Array.of_list (IntSet.elements engine.configuration)
  let get_configuration_names engine doc =
    let configuration = engine.configuration in
    IntSet.fold (fun idx acc ->
      match resolve doc idx with
      | {State.id=Some id} -> Array.append acc [| id |]
      | _ -> acc
    ) configuration [| |]

  let get_datamodel engine = engine.datamodel
  let put_datamodel engine datamodel = {engine with datamodel=datamodel}
  let get_executions engine = engine.executions
end
