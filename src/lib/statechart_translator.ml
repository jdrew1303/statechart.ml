open Statechart_translator_predicates

module Src = Statechart_t
module Tgt = Statechart_executable
module Bitset = Statechart_bitset

module StringMap = Map.Make(struct
  type t = string
  let compare = compare
end)

module IntMap = Map.Make(struct
  type t = int
  let compare = compare
end)

type child_type = State of int
                | OnEntry of Tgt.expression array
                | OnExit of Tgt.expression array
                | Transition of (unit -> int)
                | Invoke of Tgt.invoke
                | Pass

let get_parent ancestors =
  match ancestors with
  | parent :: _ -> parent
  | _ -> 0

exception UnparsedExpression of string
let unwrap_expr expr =
  match expr with
  | Src.Expr v -> raise (UnparsedExpression v)
  | Src.ExprValue v -> Tgt.string_expr v
  | Src.ExprParsed v -> v
  | Src.ExprUnset -> Tgt.null

let map_to_array a f =
  let size = IntMap.cardinal a in
  Array.init size (fun idx ->
    f (IntMap.find idx a)
  )

let set_mut map idx value =
  map := IntMap.add idx value !map

let append_mut_array arr value =
  arr := Array.append !arr value

let get_next r =
  let idx = !r in
  r := idx + 1;
  idx

let resolve_list map l =
  let targets = List.map (fun k -> StringMap.find k map) l in
  Array.of_list targets

let maybe_find map k =
  try
    Some (IntMap.find k map)
  with
  | _ -> None

let has_history children state_map =
  let n = Array.length children in
  let rec find i =
    if i >= n then false
    else (
      let {Tgt.State.t} = IntMap.find i state_map in
      (t == `history_deep || t == `history_shallow) || find (i + 1)
    )
  in
  find 0

let get_history_completion state states =
  (* TODO *)
  [||]

let find_initial states children =
  match Array.length children with
  | 0 -> [||]
  | n ->
    let rec find i =
      if i >= n then [|Array.get children 0|]
      else (
        let idx = Array.get children i in
        match IntMap.find idx states with
        | {Tgt.State.t=`initial} -> [|idx|]
        | _ -> find (i + 1)
      )
    in
    find 0

let get_completion state completions state_map states descendants =
  match state with
  (* TODO filter history children *)
  | {Tgt.State.t=`history_deep; idx; children} ->
    Array.append children (Bitset.to_idx_array (Array.get descendants idx))
  | {Tgt.State.t=`history_shallow; children} -> children
  | {Tgt.State.t=`parallel; children} -> children
  | {Tgt.State.idx=idx} -> (
    match maybe_find completions idx with
    | Some initial when (initial != []) -> resolve_list state_map initial
    | _ -> find_initial states state.Tgt.State.children
  )

let flatten document =
  let states = ref IntMap.empty in
  let state_idx = ref 0 in
  let state_map = ref StringMap.empty in
  let state_completions = ref IntMap.empty in
  let state_descendants = ref IntMap.empty in
  let transitions = ref IntMap.empty in
  let transition_idx = ref 0 in
  let transition_targets = ref IntMap.empty in
  let datamodel = ref [] in
  (* let is_early = document.Src.Document.binding == `early in *)

  let maybe_map_state idx id =
    match id with
    | None -> ()
    | Some s -> state_map := StringMap.add s idx !state_map
  in

  let rec translate_document () =
    let idx = get_next state_idx in
    let children, on_enter, on_exit, transitions, invocations =
      translate_children document.Src.Document.children idx [] in
    set_mut state_completions idx document.Src.Document.initial;
    set_mut states idx {
      Tgt.State.t=`compound;
      idx;
      id=document.Src.Document.name;
      on_enter;
      on_exit;
      invocations;
      (* TODO get children *)
      data=Array.of_list (List.rev !datamodel);
      (* TODO *)
      donedata=None;
      parent=idx;
      children;
      ancestors=[||];
      completion=[||];
      transitions;
      has_history=false;
    };

  and translate_state state ancestors =
    let idx = get_next state_idx in
    let children, on_enter, on_exit, transitions, invocations =
      translate_children state.Src.State.children idx ancestors in
    maybe_map_state idx state.Src.State.id;
    set_mut state_completions idx state.Src.State.initial;
    set_mut states idx {
      Tgt.State.t=(match children with
      | [||] -> `atomic
      | _ -> `compound);
      idx;
      id=state.Src.State.id;
      on_enter;
      on_exit;
      invocations;
      (* TODO *)
      data=[||];
      (* TODO *)
      donedata=None;
      parent=get_parent ancestors;
      children;
      ancestors=Array.of_list ancestors;
      completion=[||];
      transitions;
      has_history=false;
    };
    idx

  and translate_parallel state ancestors =
    let idx = get_next state_idx in
    let children, on_enter, on_exit, transitions, invocations =
      translate_children state.Src.Parallel.children idx ancestors in
    maybe_map_state idx state.Src.Parallel.id;
    set_mut states idx {
      Tgt.State.t=`parallel;
      idx;
      id=state.Src.Parallel.id;
      on_enter;
      on_exit;
      invocations;
      (* TODO *)
      data=[||];
      (* TODO *)
      donedata=None;
      parent=get_parent ancestors;
      children;
      ancestors=Array.of_list ancestors;
      completion=[||];
      transitions;
      has_history=false;
    };
    idx

  and translate_transition transition ancestors =
    let idx = get_next transition_idx in
    let parent = get_parent ancestors in
    let condition = match transition.Src.Transition.cond with
    | Src.ExprUnset -> None
    | cond -> Some (unwrap_expr cond) in
    set_mut transition_targets idx transition.Src.Transition.target;
    let children = translate_executables transition.Src.Transition.children in
    set_mut transitions idx {
      Tgt.Transition.t=(match transition with
      | {Src.Transition.target=[]} -> `targetless
      | {Src.Transition.t=`internal} -> `internal
      | {Src.Transition.event=[]} -> `spontaneous
      | _ -> `external_
      );
      idx;
      source=parent;
      events=Array.of_list transition.Src.Transition.event;
      condition=condition;
      on_transition=children;
      targets=[||];
      conflicts=[||];
      exits=[||];
    };
    idx

  and translate_initial state ancestors =
    let idx = get_next state_idx in
    let children, on_enter, on_exit, transitions, invocations =
      translate_children state.Src.Initial.children idx ancestors in
    set_mut states idx {
      Tgt.State.t=`initial;
      idx;
      id=None;
      on_enter;
      on_exit;
      invocations;
      (* TODO *)
      data=[||];
      (* TODO *)
      donedata=None;
      parent=get_parent ancestors;
      children;
      ancestors=Array.of_list ancestors;
      completion=[||];
      transitions;
      has_history=false;
    };
    idx

  and translate_final state ancestors =
    let idx = get_next state_idx in
    let children, on_enter, on_exit, transitions, invocations =
      translate_children state.Src.Final.children idx ancestors in
    maybe_map_state idx state.Src.Final.id;
    set_mut states idx {
      Tgt.State.t=`final;
      idx;
      id=state.Src.Final.id;
      on_enter;
      on_exit;
      invocations;
      (* TODO *)
      data=[||];
      (* TODO *)
      donedata=None;
      parent=get_parent ancestors;
      children;
      ancestors=Array.of_list ancestors;
      completion=[||];
      transitions;
      has_history=false;
    };
    idx

  and translate_history state ancestors =
    let idx = get_next state_idx in
    let children, on_enter, on_exit, transitions, invocations =
      translate_children state.Src.History.children idx ancestors in
    maybe_map_state idx state.Src.History.id;
    set_mut states idx {
      Tgt.State.t=(match state.Src.History.t with
      | `deep -> `history_deep
      | _ -> `history_shallow);
      idx;
      id=state.Src.History.id;
      on_enter;
      on_exit;
      invocations;
      (* TODO *)
      data=[||];
      (* TODO *)
      donedata=None;
      parent=get_parent ancestors;
      children;
      ancestors=Array.of_list ancestors;
      completion=[||];
      transitions;
      has_history=true;
    };
    idx

  and translate_child child ancestors =
    match child with
    | Src.State c -> State (translate_state c ancestors)
    | Src.Parallel c -> State (translate_parallel c ancestors)
    | Src.Transition c -> Transition (fun () -> translate_transition c ancestors)
    | Src.Initial c -> State (translate_initial c ancestors)
    | Src.Final c -> State (translate_final c ancestors)
    | Src.OnEntry c -> OnEntry (translate_executables c.Src.OnEntry.children)
    | Src.OnExit c -> OnExit (translate_executables c.Src.OnExit.children)
    | Src.History c -> State (translate_history c ancestors)
    (* TODO *)
    | Src.Script c -> OnEntry [||]
    (* TODO datamodel *)
    (* TODO invoke *)
    | _ -> Pass

  and sort_children a b =
    match a, b with
    | Src.Initial _, Src.Initial _ -> 0
    | Src.Initial _, _ -> -1
    | _, Src.Initial _ -> 1
    | Src.History {Src.History.t=a_t}, Src.History {Src.History.t=b_t} -> (
      match a_t, b_t with
      | `deep, `deep -> 0
      | `shallow, `shallow -> 0
      | `deep, _ -> -1
      | `shallow, _ -> 1
    )
    | Src.History _, _ -> -1
    | _, Src.History _ -> 1
    | _, _ -> 0

  and save_descendants parent children =
    let sd = !state_descendants in
    let desc = Array.fold_left (fun acc i ->
      match maybe_find sd i with
      | None -> acc
      | Some d -> Array.append acc d
    ) children children in
    state_descendants := IntMap.add parent desc sd

  and translate_children children parent ancestors =
    let ancestors = parent :: ancestors in
    let idxs = ref [||] in
    let on_enter = ref [||] in
    let on_exit = ref [||] in
    let transitions = ref [||] in
    let invocations = ref [||] in
    let children = List.stable_sort sort_children children in
    List.iter (fun child ->
      match translate_child child ancestors with
      | State idx -> append_mut_array idxs [|idx|];
      | OnEntry e -> append_mut_array on_enter e;
      | OnExit e -> append_mut_array on_exit e;
      | Transition t -> append_mut_array transitions [|t|];
      | Invoke i -> append_mut_array invocations [|i|];
      | Pass -> ()
    ) children;
    let idxs = !idxs in
    save_descendants parent idxs;
    (* post evaluate so we get the deepest transitions first *)
    let transitions = Array.map (fun t -> t ()) !transitions in
    idxs, !on_enter, !on_exit, transitions, !invocations

  and translate_raise raise =
    match raise.Src.Raise.event with
    | None -> Tgt.null
    | Some e -> Tgt.complex_expr `raise [Tgt.string_expr e]

  and translate_case case =
    let children = List.map translate_case_clause case.Src.Case.children in
    Tgt.complex_expr `case children

  and translate_case_clause clause =
    Tgt.complex_expr `clause [
      unwrap_expr clause.Src.CaseClause.cond;
      Tgt.complex_expr `block (Array.to_list (translate_executables clause.Src.CaseClause.children));
    ]

  and translate_foreach foreach =
    Tgt.complex_expr `foreach [
      unwrap_expr foreach.Src.Foreach.array;
      unwrap_expr foreach.Src.Foreach.item;
      unwrap_expr foreach.Src.Foreach.index;
      Tgt.complex_expr `block (Array.to_list (translate_executables foreach.Src.Foreach.children));
    ]

  and translate_log log =
    Tgt.complex_expr `log [
      (match log.Src.Log.label with
      | Some l -> Tgt.string_expr l
      | None -> Tgt.null);
      unwrap_expr log.Src.Log.expr;
    ]

  and translate_assign assign =
    Tgt.complex_expr `assign [
      unwrap_expr assign.Src.Assign.location;
      unwrap_expr assign.Src.Assign.expr;
    ]

  and translate_send send =
    Tgt.complex_expr `send [
      unwrap_expr send.Src.Send.event;
      unwrap_expr send.Src.Send.target;
      unwrap_expr send.Src.Send.t;
      unwrap_expr send.Src.Send.id;
      unwrap_expr send.Src.Send.delay;
      (* TODO handle namelist *)
      (* TODO handle children *)
    ]

  and translate_cancel cancel =
    Tgt.complex_expr `cancel [
      unwrap_expr cancel.Src.Cancel.sendid;
    ]

  and translate_executable child =
    match child with
    | Src.Raise e -> [|translate_raise e|]
    | Src.Case e -> [|translate_case e|]
    | Src.Foreach e -> [|translate_foreach e|]
    | Src.Log e -> [|translate_log e|]
    (* TODO datamodel? *)
    | Src.Assign e -> [|translate_assign e|]
    (* TODO done data? *)
    (* TODO content? *)
    (* TODO param? *)
    | Src.Send e -> [|translate_send e|]
    | Src.Cancel e -> [|translate_cancel e|]
    (* TODO invoke? *)
    | Src.Finalize e -> translate_executables e.Src.Finalize.children
    | _ -> [||]

  and translate_executables children =
    List.fold_right (fun child acc ->
      Array.append (translate_executable child) acc
    ) children [||]

  in

  translate_document ();

  let states = !states in
  let state_map = !state_map in
  let state_bitset = Bitset.of_idx_array (IntMap.cardinal states) in
  let state_completions = !state_completions in
  let descendants = map_to_array !state_descendants state_bitset in

  let transitions = !transitions in
  let transition_targets = !transition_targets in
  let transition_count = IntMap.cardinal transitions in

  let states = map_to_array states (fun s ->
    let completion = get_completion s state_completions state_map states descendants in
    let children = s.Tgt.State.children in
    {s with
      Tgt.State.completion=state_bitset completion;
      ancestors=state_bitset s.Tgt.State.ancestors;
      children=state_bitset children;
      has_history=s.Tgt.State.has_history || (has_history children states);
      transitions=Bitset.of_idx_array transition_count s.Tgt.State.transitions;
    }
  ) in

  let transitions = map_to_array transitions (fun t ->
    let targets = IntMap.find t.Tgt.Transition.idx transition_targets in
    let target_ids = resolve_list state_map targets in
    let t = resolve_transition_type states t in
    {t with
      Tgt.Transition.targets=state_bitset target_ids;
      exits=get_exit_set descendants states t target_ids;
    }
  ) in

  (* compute the conflicts *)
  let transitions = Array.map (fun t1 ->
    let conflicts = Bitset.init (fun i ->
      let t2 = Array.get transitions i in
      has_conflict descendants states t1 t2
    ) transition_count in
    {t1 with Tgt.Transition.conflicts}
  ) transitions in

  states, transitions

let translate document =
  let states, transitions = flatten document in
  {
    Tgt.Document.name=document.Src.Document.name;
    states=states;
    transitions=transitions;
  }
