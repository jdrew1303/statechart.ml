open Statechart_interpreter_engine

module IntSet = Set.Make(struct
  type t = int
  let compare = compare
end)

module type Interpreter = sig
  type engine
  type datamodel
  type document
  type executable
  type event
  val start : datamodel -> document -> engine
  val handle_internal_event : engine -> document -> event -> engine
  val synchronize : engine -> document -> engine
  val handle_external_event : engine -> document -> event -> engine
  val finalize_macrostep : engine -> document -> engine
  val stop : engine -> document -> engine

  type configuration
  type internal
  type executions
  type invocations
  val get_configuration : engine -> configuration
  val get_configuration_names : engine -> document -> string array
  val get_datamodel : engine -> datamodel
  val put_datamodel : engine -> datamodel -> engine
  val get_invocations : engine -> invocations
  val get_queues : engine -> (internal * executions)
  val is_running : engine -> bool
end

module Make(Eng : Engine) = struct
  type datamodel = Eng.datamodel
  type configuration = IntSet.t
  type executable = Eng.executable
  type event_pattern = Eng.event_pattern
  type event = string
  type query = datamodel -> bool

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
    end = TYPES
  and Document:
    sig
      type t = {
        name: string option;
        initial_transitions: TYPES.transition list;
        states: TYPES.state array;
      }
    end = Document
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
        idx: int;
        depth: int;
        order: int;
        id: string option;
        t: TYPES.state_type;
        initial_state: int option;
        transitions: TYPES.transition array;
        invocations: TYPES.invoke array;
        on_enter: executable array;
        on_exit: executable array;
        children: IntSet.t;
        parent: int option;
        ancestors: IntSet.t;
        descendants: IntSet.t;
        history: int array;
        history_type: TYPES.history_type option;
        donedata: executable option;
      }
    end = State
  and Transition:
    sig
      type t = {
        domain: int;
        depth: int;
        order: int;
        source: int;
        targets: int array;
        events: event_pattern option;
        condition: query option;
        t: TYPES.transition_type;
        on_transition: executable array;
      }
    end = Transition

  type document = TYPES.document
  type invoke = TYPES.invoke
  type param = TYPES.param
  type state = TYPES.state
  type transition = TYPES.transition

  module HistoryMap = Map.Make(struct
    type t = int
    let compare = compare
  end)

  type internal = (event * executable option) array
  type executions = executable array
  type invocations = invoke array

  type engine = {
    configuration: IntSet.t;
    datamodel: datamodel;
    history: IntSet.t HistoryMap.t;
    internal: internal;
    executions: executions;
    invocations: invocations;
    is_running: bool;
  }

  module EnterStates = Set.Make(struct
    type t = State.t

    let compare s1 s2 =
      match s1.State.depth, s2.State.depth with
      | d1, d2 when d1 = d2 -> Pervasives.compare s1.State.order s2.State.order
      | d1, d2 when d1 >= d2 -> 1
      | _ -> -1
  end)

  module ExitStates = Set.Make(struct
    type t = State.t

    let compare s1 s2 =
      match s1.State.depth, s2.State.depth with
      | d1, d2 when d1 = d2 -> Pervasives.compare s1.State.order s2.State.order
      | d1, d2 when d2 >= d1 -> -1
      | _ -> 1
  end)

  module TransitionSet = Set.Make(struct
    type t = Transition.t

    let compare s1 s2 =
      match s1.Transition.depth, s2.Transition.depth with
      | d1, d2 when d1 = d2 -> Pervasives.compare s1.Transition.order s2.Transition.order
      | d1, d2 when d1 >= d2 -> 1
      | _ -> -1
  end)

  let resolve doc idx = Array.get doc.Document.states idx
  let resolve_set doc set =
    IntSet.fold (fun idx acc ->
      EnterStates.add (resolve doc idx) acc
    ) set EnterStates.empty

  (* This is pretty imperative but it'll probably be faster for most cases *)
  let array_find cond arr =
    let value = ref None in
    let loop = ref true in
    let i = ref 0 in
    let l = Array.length arr in
    while !loop && !i < l do
      let v = Array.get arr !i in
      if cond v then (
        loop := false;
        value := Some v
      );
      incr i
    done;
    !value

  (*
  function removeConflictingTransitions(enabledTransitions)

  enabledTransitions will contain multiple transitions only if a parallel state is active. In that case, we may have one transition selected for each of its children. These transitions may conflict with each other in the sense that they have incompatible target states. Loosely speaking, transitions are compatible when each one is contained within a single <state> child of the <parallel> element. Transitions that aren't contained within a single child force the state machine to leave the <parallel> ancestor (even if they reenter it later). Such transitions conflict with each other, and with transitions that remain within a single <state> child, in that they may have targets that cannot be simultaneously active. The test that transitions have non-intersecting exit sets captures this requirement. (If the intersection is null, the source and targets of the two transitions are contained in separate <state> descendants of <parallel>. If intersection is non-null, then at least one of the transitions is exiting the <parallel>). When such a conflict occurs, then if the source state of one of the transitions is a descendant of the source state of the other, we select the transition in the descendant. Otherwise we prefer the transition that was selected by the earlier state in document order and discard the other transition. Note that targetless transitions have empty exit sets and thus do not conflict with any other transitions.

  We start with a list of enabledTransitions and produce a conflict-free list of filteredTransitions. For each t1 in enabledTransitions, we test it against all t2 that are already selected in filteredTransitions. If there is a conflict, then if t1's source state is a descendant of t2's source state, we prefer t1 and say that it preempts t2 (so we we make a note to remove t2 from filteredTransitions). Otherwise, we prefer t2 since it was selected in an earlier state in document order, so we say that it preempts t1. (There's no need to do anything in this case since t2 is already in filteredTransitions. Furthermore, once one transition preempts t1, there is no need to test t1 against any other transitions.) Finally, if t1 isn't preempted by any transition in filteredTransitions, remove any transitions that it preempts and add it to that list.

  function removeConflictingTransitions(enabledTransitions):
      filteredTransitions = new OrderedSet()
      //toList sorts the transitions in the order of the states that selected them
      for t1 in enabledTransitions.toList():
          t1Preempted = false
          transitionsToRemove = new OrderedSet()
          for t2 in filteredTransitions.toList():
              if computeExitSet([t1]).hasIntersection(computeExitSet([t2])):
                  if isDescendant(t1.source, t2.source):
                      transitionsToRemove.add(t2)
                  else:
                      t1Preempted = true
                      break
          if not t1Preempted:
              for t3 in transitionsToRemove.toList():
                  filteredTransitions.delete(t3)
              filteredTransitions.add(t1)

      return filteredTransitions
  *)

  (* TODO *)
  let remove_conflicting_transitions doc t =
    t

  let query_transitions filter doc conf =
    (* TODO catch any exceptions and put them on the internal queue *)
    let t = IntSet.fold (fun idx acc ->
      let state = resolve doc idx in
      if IntSet.is_empty state.State.children
      then (
        let ancs = state.State.children in
        EnterStates.fold (fun state acc ->
          match array_find filter state.State.transitions with
          | Some transition -> TransitionSet.add transition acc
          | None -> acc
        ) (EnterStates.add state (resolve_set doc ancs)) acc
      )
      else acc
    ) conf TransitionSet.empty in
    remove_conflicting_transitions doc t

  (*
  function selectEventlessTransitions()

  This function selects all transitions that are enabled in the current configuration that do not require an event trigger. First find a transition with no 'event' attribute whose condition evaluates to true. If multiple matching transitions are present, take the first in document order. If none are present, search in the state's ancestors in ancestry order until one is found. As soon as such a transition is found, add it to enabledTransitions, and proceed to the next atomic state in the configuration. If no such transition is found in the state or its ancestors, proceed to the next state in the configuration. When all atomic states have been visited and transitions selected, filter the set of enabled transitions, removing any that are preempted by other transitions, then return the resulting set.

  function selectEventlessTransitions():
      enabledTransitions = new OrderedSet()
      atomicStates = configuration.toList().filter(isAtomicState).sort(documentOrder)
      for state in atomicStates:
          loop: for s in [state].append(getProperAncestors(state, null)):
              for t in s.transition.sort(documentOrder):
                  if not t.event and conditionMatch(t):
                      enabledTransitions.add(t)
                      break loop
      enabledTransitions = removeConflictingTransitions(enabledTransitions)
      return enabledTransitions
  *)

  let select_eventless_transitions engine doc conf =
    let datamodel = engine.datamodel in
    query_transitions (fun transition ->
      match transition with
      | {Transition.events=None; condition=Some cond} ->
        cond datamodel
      | {Transition.events=None; condition=None} -> true
      | _ -> false
    ) doc conf

  (*
  function selectTransitions(event)

  The purpose of the selectTransitions()procedure is to collect the transitions that are enabled by this event in the current configuration.

  Create an empty set of enabledTransitions. For each atomic state , find a transition whose 'event' attribute matches event and whose condition evaluates to true. If multiple matching transitions are present, take the first in document order. If none are present, search in the state's ancestors in ancestry order until one is found. As soon as such a transition is found, add it to enabledTransitions, and proceed to the next atomic state in the configuration. If no such transition is found in the state or its ancestors, proceed to the next state in the configuration. When all atomic states have been visited and transitions selected, filter out any preempted transitions and return the resulting set.

  function selectTransitions(event):
      enabledTransitions = new OrderedSet()
      atomicStates = configuration.toList().filter(isAtomicState).sort(documentOrder)
      for state in atomicStates:
          loop: for s in [state].append(getProperAncestors(state, null)):
              for t in s.transition.sort(documentOrder):
                  if t.event and nameMatch(t.event, event.name) and conditionMatch(t):
                      enabledTransitions.add(t)
                      break loop
      enabledTransitions = removeConflictingTransitions(enabledTransitions)
      return enabledTransitions
  *)

  let select_transitions engine doc conf event =
    let datamodel = engine.datamodel in
    query_transitions (fun transition ->
      match transition with
      | {Transition.events=Some events; condition=Some cond} ->
        (Eng.match_event events event) && (cond datamodel)
      | {Transition.events=Some events; condition=None} ->
        Eng.match_event events event
      | _ -> false
    ) doc conf

  (*
  procedure computeExitSet(enabledTransitions)

  For each transition t in enabledTransitions, if t is targetless then do nothing, else compute the transition's domain. (This will be the source state in the case of internal transitions) or the least common compound ancestor state of the source state and target states of t (in the case of external transitions. Add to the statesToExit set all states in the configuration that are descendants of the domain.

  function computeExitSet(transitions)
      statesToExit = new OrderedSet
      for t in transitions:
          if t.target:
              domain = getTransitionDomain(t)
              for s in configuration:
                  if isDescendant(s,domain):
                      statesToExit.add(s)
      return statesToExit
  *)

  let compute_exit_set doc conf enabled =
    TransitionSet.fold (fun transition acc ->
      match transition with
      | {Transition.targets=[||]} -> acc
      | {Transition.targets=targets; domain=domain} ->
        Array.fold_left (fun acc target ->
          IntSet.fold (fun idx acc ->
            let state = resolve doc idx in
            if IntSet.mem domain state.State.descendants
            then ExitStates.add state acc
            else acc
          ) conf acc
        ) acc targets
    ) enabled ExitStates.empty

  (*
  procedure exitStates(enabledTransitions)

  Compute the set of states to exit. Then remove all the states on statesToExit from the set of states that will have invoke processing done at the start of the next macrostep. (Suppose macrostep M1 consists of microsteps m11 and m12. We may enter state s in m11 and exit it in m12. We will add s to statesToInvoke in m11, and must remove it in m12. In the subsequent macrostep M2, we will apply invoke processing to all states that were entered, and not exited, in M1.) Then convert statesToExit to a list and sort it in exitOrder.

  For each state s in the list, if s has a deep history state h, set the history value of h to be the list of all atomic descendants of s that are members in the current configuration, else set its value to be the list of all immediate children of s that are members of the current configuration. Again for each state s in the list, first execute any onexit handlers, then cancel any ongoing invocations, and finally remove s from the current configuration.

  procedure exitStates(enabledTransitions):
      statesToExit = computeExitSet(enabledTransitions)
      for s in statesToExit:
          statesToInvoke.delete(s)
      statesToExit = statesToExit.toList().sort(exitOrder)
      for s in statesToExit:
          for h in s.history:
              if h.type == "deep":
                  f = lambda s0: isAtomicState(s0) and isDescendant(s0,s)
              else:
                  f = lambda s0: s0.parent == s
              historyValue[h.id] = configuration.toList().filter(f)
      for s in statesToExit:
          for content in s.onexit.sort(documentOrder):
              executeContent(content)
          for inv in s.invoke:
              cancelInvoke(inv)
          configuration.delete(s)
  *)

  let remember_history engine conf to_exit =
    ExitStates.fold (fun state acc ->
      match state with
      | {State.history=hist; history_type=Some `deep} ->
        (* TODO *)
        acc
      | {State.history=hist; history_type=Some `shallow} ->
        (* TODO *)
        acc
      | _ ->
        acc
    ) to_exit engine

  let exit_states engine doc conf enabled =
    let to_exit = compute_exit_set doc conf enabled in
    let engine = remember_history engine conf to_exit in
    let exec, inv, conf = ExitStates.fold (fun state acc ->
      let exec, inv, conf = acc in
      let exec = Array.append exec state.State.on_exit in
      let inv = Array.append inv state.State.invocations in
      let conf = IntSet.remove state.State.idx conf in
      exec, inv, conf
    ) to_exit (engine.executions, engine.invocations, conf) in
    {engine with executions=exec; invocations=inv}, conf

  (*
  procedure executeTransitionContent(enabledTransitions)

  For each transition in the list of enabledTransitions, execute its executable content.

  procedure executeTransitionContent(enabledTransitions):
      for t in enabledTransitions:
          executeContent(t)
  *)

  let execute_transition_content engine enabled =
    let executions = TransitionSet.fold (fun transition exec ->
      Array.append exec transition.Transition.on_transition
    ) enabled engine.executions in
    {engine with executions=executions}

  (*
  function getProperAncestors(state1, state2)

  If state2 is null, returns the set of all ancestors of state1 in ancestry order (state1's parent followed by the parent's parent, etc. up to an including the <scxml> element). If state2 is non-null, returns in ancestry order the set of all ancestors of state1, up to but not including state2. (A "proper ancestor" of a state is its parent, or the parent's parent, or the parent's parent's parent, etc.))If state2 is state1's parent, or equal to state1, or a descendant of state1, this returns the empty set.
  *)

  let get_proper_ancestors s1 s2 =
    let s1_a = s1.State.ancestors in
    let s2_a = s2.State.ancestors in
    IntSet.diff (IntSet.add s2.State.idx s2_a) s1_a

  (*
  procedure addDescendantStatesToEnter(state,statesToEnter,statesForDefaultEntry, defaultHistoryContent)

  The purpose of this procedure is to add to statesToEnter 'state' and any of its descendants that the state machine will end up entering when it enters 'state'. (N.B. If 'state' is a history pseudo-state, we dereference it and add the history value instead.) Note that this procedure permanently modifies both statesToEnter and statesForDefaultEntry.

  First, If state is a history state then add either the history values associated with state or state's default target to statesToEnter. Then (since the history value may not be an immediate descendant of 'state's parent) add any ancestors between the history value and state's parent. Else (if state is not a history state), add state to statesToEnter. Then if state is a compound state, add state to statesForDefaultEntry and recursively call addStatesToEnter on its default initial state(s). Then, since the default initial states may not be children of 'state', add any ancestors between the default initial states and 'state'. Otherwise, if state is a parallel state, recursively call addStatesToEnter on any of its child states that don't already have a descendant on statesToEnter.

  procedure addDescendantStatesToEnter(state,statesToEnter,statesForDefaultEntry, defaultHistoryContent):
      if isHistoryState(state):
          if historyValue[state.id]:
              for s in historyValue[state.id]:
                  addDescendantStatesToEnter(s,statesToEnter,statesForDefaultEntry, defaultHistoryContent)
              for s in historyValue[state.id]:
                  addAncestorStatesToEnter(s, state.parent, statesToEnter, statesForDefaultEntry, defaultHistoryContent)
          else:
              defaultHistoryContent[state.parent.id] = state.transition.content
              for s in state.transition.target:
                  addDescendantStatesToEnter(s,statesToEnter,statesForDefaultEntry, defaultHistoryContent)
              for s in state.transition.target:
                  addAncestorStatesToEnter(s, state.parent, statesToEnter, statesForDefaultEntry, defaultHistoryContent)
      else:
          statesToEnter.add(state)
          if isCompoundState(state):
              statesForDefaultEntry.add(state)
              for s in state.initial.transition.target:
                  addDescendantStatesToEnter(s,statesToEnter,statesForDefaultEntry, defaultHistoryContent)
              for s in state.initial.transition.target:
                  addAncestorStatesToEnter(s, state, statesToEnter, statesForDefaultEntry, defaultHistoryContent)
          else:
              if isParallelState(state):
                  for child in getChildStates(state):
                      if not statesToEnter.some(lambda s: isDescendant(s,child)):
                          addDescendantStatesToEnter(child,statesToEnter,statesForDefaultEntry, defaultHistoryContent)
  *)

  (* TODO *)
  let rec add_descendant_states_to_enter engine doc to_enter state =
    to_enter

  (*
  procedure addAncestorStatesToEnter(state, ancestor, statesToEnter, statesForDefaultEntry, defaultHistoryContent)

  Add to statesToEnter any ancestors of 'state' up to, but not including, 'ancestor' that must be entered in order to enter 'state'. If any of these ancestor states is a parallel state, we must fill in its descendants as well.

  procedure addAncestorStatesToEnter(state, ancestor, statesToEnter, statesForDefaultEntry, defaultHistoryContent)
      for anc in getProperAncestors(state,ancestor):
          statesToEnter.add(anc)
          if isParallelState(anc):
              for child in getChildStates(anc):
                  if not statesToEnter.some(lambda s: isDescendant(s,child)):
                      addDescendantStatesToEnter(child,statesToEnter,statesForDefaultEntry, defaultHistoryContent)
  *)

  and add_ancestor_states_to_enter engine doc to_enter state ancestor =
    IntSet.fold (fun idx acc ->
      let anc = resolve doc idx in
      let acc = EnterStates.add anc acc in
      match anc with
      | {State.t=`parallel; children=children} ->
        IntSet.fold (fun child_idx acc ->
          (* TODO *)
          acc
        ) children acc
      | _ -> acc
    ) (get_proper_ancestors state ancestor) to_enter

  (*
  function getEffectiveTargetStates(transition)

  Returns the states that will be the target when 'transition' is taken, dereferencing any history states.

  function getEffectiveTargetStates(transition)
      targets = new OrderedSet()
      for s in transition.target
          if isHistoryState(s):
              if historyValue[s.id]:
                  targets.union(historyValue[s.id])
              else:
                  targets.union(getEffectiveTargetStates(s.transition))
          else:
              targets.add(s)
      return targets
  *)

  let get_history history key =
    try Some (HistoryMap.find key history)
    with
    | _ -> None

  let rec get_effective_target_states engine doc transition =
    let acc = EnterStates.empty in
    let history = engine.history in
    Array.fold_left (fun acc idx ->
      let state = resolve doc idx in
      match state with
      | {State.t=`history; transitions=transitions} -> (
        match get_history history idx with
        | None -> (
          Array.fold_left (fun acc transition ->
            let targets = get_effective_target_states engine doc transition in
            EnterStates.union acc targets
          ) acc transitions
        )
        | Some hist -> (
          let states = resolve_set doc hist in
          EnterStates.union acc states
        )
      )
      | _ -> EnterStates.add state acc
    ) acc transition.Transition.targets

  (*
  procedure computeEntrySet(transitions, statesToEnter, statesForDefaultEntry, defaultHistoryContent)

  Compute the complete set of states that will be entered as a result of taking 'transitions'. This value will be returned in 'statesToEnter' (which is modified by this procedure). Also place in 'statesForDefaultEntry' the set of all states whose default initial states were entered. First gather up all the target states in 'transitions'. Then add them and, for all that are not atomic states, add all of their (default) descendants until we reach one or more atomic states. Then add any ancestors that will be entered within the domain of the transition. (Ancestors outside of the domain of the transition will not have been exited.)

  procedure computeEntrySet(transitions, statesToEnter, statesForDefaultEntry, defaultHistoryContent)
      for t in transitions:
          for s in t.target:
              addDescendantStatesToEnter(s,statesToEnter,statesForDefaultEntry, defaultHistoryContent)
          ancestor = getTransitionDomain(t)
          for s in getEffectiveTargetStates(t)):
              addAncestorStatesToEnter(s, ancestor, statesToEnter, statesForDefaultEntry, defaultHistoryContent)
  *)

  let compute_entry_set engine doc conf transitions =
    TransitionSet.fold (fun transition acc ->
      Array.fold_left (fun acc idx ->
        let state = resolve doc idx in
        let acc = add_descendant_states_to_enter engine doc acc state in
        let ancestor = resolve doc transition.Transition.domain in
        let effective_target_states = get_effective_target_states engine doc transition in
        EnterStates.fold (fun state acc ->
          add_ancestor_states_to_enter engine doc acc state ancestor
        ) effective_target_states acc
      ) acc transition.Transition.targets
    ) transitions EnterStates.empty

  (*
  procedure isInFinalState(s)

  Return true if s is a compound <state> and one of its children is an active <final> state (i.e. is a member of the current configuration), or if s is a <parallel> state and isInFinalState is true of all its children.

  function isInFinalState(s):
      if isCompoundState(s):
          return getChildStates(s).some(lambda s: isFinalState(s) and configuration.isMember(s))
      elif isParallelState(s):
          return getChildStates(s).every(isInFinalState)
      else:
          return false
  *)

  let rec is_in_final_state doc conf idx =
    match resolve doc idx with
    | {State.t=`compound; children=children} ->
      IntSet.exists (fun idx ->
        IntSet.mem idx conf && is_in_final_state doc conf idx
      ) children
    | {State.t=`parallel; children=children} -> IntSet.for_all (is_in_final_state doc conf) children
    | _ -> false

  (*
  procedure enterStates(enabledTransitions)

  First, compute the list of all the states that will be entered as a result of taking the transitions in enabledTransitions. Add them to statesToInvoke so that invoke processing can be done at the start of the next macrostep. Convert statesToEnter to a list and sort it in entryOrder. For each state s in the list, first add s to the current configuration. Then if we are using late binding, and this is the first time we have entered s, initialize its data model. Then execute any onentry handlers. If s's initial state is being entered by default, execute any executable content in the initial transition. If a history state in s was the target of a transition, and s has not been entered before, execute the content inside the history state's default transition. Finally, if s is a final state, generate relevant Done events. If we have reached a top-level final state, set running to false as a signal to stop processing.

  procedure enterStates(enabledTransitions):
      statesToEnter = new OrderedSet()
      statesForDefaultEntry = new OrderedSet()
      // initialize the temporary table for default content in history states
      defaultHistoryContent = new HashTable()
      computeEntrySet(enabledTransitions, statesToEnter, statesForDefaultEntry, defaultHistoryContent)
      for s in statesToEnter.toList().sort(entryOrder):
          configuration.add(s)
          statesToInvoke.add(s)
          ANALYZE:
          if binding == "late" and s.isFirstEntry:
              initializeDataModel(datamodel.s,doc.s)
              s.isFirstEntry = false
          for content in s.onentry.sort(documentOrder):
              executeContent(content)
          ANALYZE:
          if statesForDefaultEntry.isMember(s):
              executeContent(s.initial.transition)
          ANALYZE:
          if defaultHistoryContent[s.id]:
              executeContent(defaultHistoryContent[s.id])
          if isFinalState(s):
              if isSCXMLElement(s.parent):
                  running = false
              else:
                  parent = s.parent
                  grandparent = parent.parent
                  internalQueue.enqueue(new Event("done.state." + parent.id, s.donedata))
                  if isParallelState(grandparent):
                      if getChildStates(grandparent).every(isInFinalState):
                          internalQueue.enqueue(new Event("done.state." + grandparent.id))
  *)

  let send_done_event engine state =
    match state with
    | {State.id=Some id} ->
      let event = (("done.state." ^ id), state.State.donedata) in
      let internal = Array.append engine.internal [| event |] in
      {engine with internal=internal}
    | _ -> engine

  let enter_states engine doc conf transitions =
    let to_enter = compute_entry_set engine doc conf transitions in
    EnterStates.fold (fun state acc ->
      let engine, conf = acc in
      let conf = IntSet.add state.State.idx conf in
      let exec = Array.append engine.executions state.State.on_enter in
      let engine = match state with
      | {State.t=`final; parent=None} -> {engine with is_running=false}
      | {State.t=`final; parent=Some p} -> (
        let parent = resolve doc p in
        let engine = send_done_event engine state in
        match parent with
        | {State.parent=Some gp} -> (
          let grandparent = resolve doc gp in
          match grandparent with
          | {State.t=`parallel; children=children} ->
            if IntSet.for_all (is_in_final_state doc conf) children
            then send_done_event engine grandparent
            else engine
          | _ -> engine
        )
        | _ -> engine
      )
      | _ -> engine in
      {engine with executions=exec}, conf
    ) to_enter (engine, conf)

  (*
  procedure microstep(enabledTransitions)

  The purpose of the microstep procedure is to process a single set of transitions. These may have been enabled by an external event, an internal event, or by the presence or absence of certain values in the data model at the current point in time. The processing of the enabled transitions must be done in parallel ('lock step') in the sense that their source states must first be exited, then their actions must be executed, and finally their target states entered.

  If a single atomic state is active, then enabledTransitions will contain only a single transition. If multiple states are active (i.e., we are in a parallel region), then there may be multiple transitions, one per active atomic state (though some states may not select a transition.) In this case, the transitions are taken in the document order of the atomic states that selected them.

  procedure microstep(enabledTransitions):
      exitStates(enabledTransitions)
      executeTransitionContent(enabledTransitions)
      enterStates(enabledTransitions)
  *)

  let microstep engine doc conf transitions =
    if TransitionSet.is_empty transitions
    then engine, conf
    else
      let engine, conf = exit_states engine doc conf transitions in
      let engine = execute_transition_content engine transitions in
      enter_states engine doc conf transitions

  (*
  function getTransitionDomain(transition)

  Return the compound state such that 1) all states that are exited or entered as a result of taking 'transition' are descendants of it 2) no descendant of it has this property.

  function getTransitionDomain(t)
      tstates = getEffectiveTargetStates(t)
      if not tstates:
          return null
      elif t.type == "internal" and isCompoundState(t.source) and tstates.every(lambda s: isDescendant(s,t.source)):
          return t.source
      else:
          return findLCCA([t.source].append(tstates))
  *)

  (* TODO *)

  (*
  function findLCCA(stateList)

  The Least Common Compound Ancestor is the <state> or <scxml> element s such that s is a proper ancestor of all states on stateList and no descendant of s has this property. Note that there is guaranteed to be such an element since the <scxml> wrapper element is a common ancestor of all states. Note also that since we are speaking of proper ancestor (parent or parent of a parent, etc.) the LCCA is never a member of stateList.

  function findLCCA(stateList):
      for anc in getProperAncestors(stateList.head(),null).filter(isCompoundStateOrScxmlElement):
          if stateList.tail().every(lambda s: isDescendant(s,anc)):
              return anc
  *)

  (* TODO *)

  (* Public interface *)

  let start datamodel doc =
    let engine = {
      configuration=IntSet.empty;
      datamodel=datamodel;
      history=HistoryMap.empty;
      internal=[||];
      executions=[||];
      invocations=[||];
      is_running=true;
    } in
    let transitions = TransitionSet.of_list doc.Document.initial_transitions in
    let engine, conf = enter_states engine doc engine.configuration transitions in
    {engine with configuration=conf}

  let handle_internal_event engine doc event =
    let conf = engine.configuration in
    let enabled = select_transitions engine doc conf event in
    let engine, conf = microstep engine doc conf enabled in
    {engine with configuration=conf}

  let synchronize engine doc =
    let engine = {engine with
      internal=[||];
      executions=[||];
    } in
    let conf = engine.configuration in
    let enabled = select_eventless_transitions engine doc conf in
    let engine, conf = microstep engine doc conf enabled in
    {engine with configuration=conf}

  let finalize_macrostep engine doc =
    let invocations = IntSet.fold (fun idx inv ->
      let state = resolve doc idx in
      Array.append inv state.State.invocations
    ) engine.configuration engine.invocations in
    {engine with invocations=invocations}

  let handle_external_event engine doc event =
    let conf = engine.configuration in
    let enabled = select_transitions engine doc conf event in
    let engine, conf = microstep engine doc conf enabled in
    {engine with configuration=conf}

  let stop engine doc =
    let exec = IntSet.fold (fun idx exec ->
      let state = resolve doc idx in
      Array.append exec state.State.on_exit
    ) engine.configuration engine.executions in
    {engine with executions=exec; invocations=[||]}

  let get_configuration engine = engine.configuration
  let get_configuration_names engine doc =
    let configuration = engine.configuration in
    IntSet.fold (fun idx acc ->
      match resolve doc idx with
      | {State.id=Some id} -> Array.append acc [| id |]
      | _ -> acc
    ) configuration [| |]

  let get_datamodel engine = engine.datamodel
  let put_datamodel engine datamodel = {engine with datamodel=datamodel}
  let get_invocations engine = engine.invocations
  let put_invocations engine invocations = {engine with invocations=invocations}
  let get_queues engine = engine.internal, engine.executions
  let is_running engine = engine.is_running
end
