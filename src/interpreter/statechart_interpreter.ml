open Statechart_interpreter_engine

module type Interpreter = sig
  type t
  type configuration
  type document
  type executable
  type event
  val start : t -> document -> t * configuration
  val handle_internal_event : t -> document -> configuration -> event -> t * configuration
  val handle_external_event : t -> document -> configuration -> event -> t * configuration
  val finalize_macrostep : t -> document -> configuration -> t
  val stop : t -> document -> configuration -> t
end

module Make(Eng : Engine) = struct
  include Eng

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
      match s2.State.depth, s1.State.depth with
      | d1, d2 when d1 = d2 -> Pervasives.compare s2.State.order s1.State.order
      | d1, d2 when d1 >= d2 -> -1
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
  let resolve_list doc list = List.map (resolve doc) list
  let resolve_set doc set =
    IntSet.fold (fun idx acc ->
      EnterStates.add (resolve doc idx) acc
    ) set EnterStates.empty

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

  let query_transitions filter engine doc conf =
    let f = filter engine in
    let t = IntSet.fold (fun idx acc ->
      let state = resolve doc idx in
      if IntSet.is_empty state.State.children
      then (
        let ancs = state.State.children in
        EnterStates.fold (fun state acc ->
          try
            let transition = List.find f state.State.transitions in
            TransitionSet.add transition acc
          with
          | _ -> acc
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
    query_transitions (fun engine transition ->
      match transition with
      | {Transition.events=[]; condition=Some cond} -> Eng.query engine cond
      | {Transition.events=[]; condition=None} -> true
      | _ -> false
    ) engine doc conf

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
    query_transitions (fun engine transition ->
      match transition with
      | {Transition.events=[]} -> false
      | {Transition.events=events; condition=Some cond} -> (Eng.match_event events event) && (Eng.query engine cond)
      | {Transition.events=events; condition=None} -> Eng.match_event events event
    ) engine doc conf

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
      | {Transition.targets=[]} -> acc
      | {Transition.targets=targets; domain=domain} ->
        List.fold_left (fun acc target ->
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
    ExitStates.fold (fun state acc ->
      let engine, new_conf = acc in
      let engine = List.fold_left Eng.execute engine state.State.on_exit in
      let engine = List.fold_left Eng.cancel engine state.State.invocations in
      let new_conf = IntSet.remove state.State.idx new_conf in
      engine, new_conf
    ) to_exit (engine, conf)

  (*
  procedure executeTransitionContent(enabledTransitions)

  For each transition in the list of enabledTransitions, execute its executable content.

  procedure executeTransitionContent(enabledTransitions):
      for t in enabledTransitions:
          executeContent(t)
  *)

  let execute_transition_content engine enabled =
    TransitionSet.fold (fun transition acc ->
      List.fold_left Eng.execute acc transition.Transition.on_transition
    ) enabled engine

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

  (* TODO *)

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

  (* TODO *)

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
          if binding == "late" and s.isFirstEntry:
              initializeDataModel(datamodel.s,doc.s)
              s.isFirstEntry = false
          for content in s.onentry.sort(documentOrder):
              executeContent(content)
          if statesForDefaultEntry.isMember(s):
              executeContent(s.initial.transition)
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

  (* TODO *)
  let enter_states engine doc conf transitions =
    engine, conf

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

  (* TODO *)

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

  (* TODO *)

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

  (* TODO *)

  (* Public interface *)

  let start engine doc =
    let transitions = TransitionSet.of_list doc.Document.initial_transitions in
    let conf = IntSet.empty in
    let engine, conf = enter_states engine doc conf transitions in
    let enabled = select_eventless_transitions engine doc conf in
    microstep engine doc conf enabled

  let handle_internal_event engine doc conf event =
    engine, conf

  let finalize_macrostep engine doc conf =
    IntSet.fold (fun idx acc ->
      let state = resolve doc idx in
      List.fold_left Eng.invoke acc state.State.invocations
    ) conf engine

  let handle_external_event engine doc conf event =
    let enabled = select_transitions engine doc conf event in
    microstep engine doc conf enabled

  let stop engine doc conf =
    IntSet.fold (fun idx engine ->
      let state = resolve doc idx in
      let engine = List.fold_left Eng.execute engine state.State.on_exit in
      List.fold_left Eng.cancel engine state.State.invocations
    ) conf engine
end
