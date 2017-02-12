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
                | Transition of int
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

let flatten document =
  let states = ref IntMap.empty in
  let state_idx = ref 0 in
  let state_map = ref StringMap.empty in
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
      translate_children document.Src.Document.children [idx] in
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
    };

  and translate_state state ancestors =
    let idx = get_next state_idx in
    let children, on_enter, on_exit, transitions, invocations =
      translate_children state.Src.State.children (idx :: ancestors) in
    maybe_map_state idx state.Src.State.id;
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
    };
    idx

  and translate_parallel state ancestors =
    let idx = get_next state_idx in
    let children, on_enter, on_exit, transitions, invocations =
      translate_children state.Src.Parallel.children (idx :: ancestors) in
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
      Tgt.Transition.t=`targetless;
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
      translate_children state.Src.Initial.children (idx :: ancestors) in
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
    };
    idx

  and translate_final state ancestors =
    let idx = get_next state_idx in
    let children, on_enter, on_exit, transitions, invocations =
      translate_children state.Src.Final.children (idx :: ancestors) in
    maybe_map_state idx state.Src.Final.id;
    set_mut states idx {
      Tgt.State.t=`parallel;
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
    };
    idx

  and translate_history state ancestors =
    let idx = get_next state_idx in
    let children, on_enter, on_exit, transitions, invocations =
      translate_children state.Src.History.children (idx :: ancestors) in
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
    };
    idx

  and translate_child child ancestors =
    match child with
    | Src.State c -> State (translate_state c ancestors)
    | Src.Parallel c -> State (translate_parallel c ancestors)
    | Src.Transition c -> Transition (translate_transition c ancestors)
    | Src.Initial c -> State (translate_initial c ancestors)
    | Src.Final c -> State (translate_final c ancestors)
    | Src.OnEntry c -> OnEntry (translate_executables c.Src.OnEntry.children)
    | Src.OnExit c -> OnExit (translate_executables c.Src.OnExit.children)
    | Src.History c -> State (translate_history c ancestors)
    (* TODO datamodel *)
    (* TODO invoke *)
    | _ -> Pass

  and translate_children children ancestors =
    let idxs = ref [||] in
    let on_enter = ref [||] in
    let on_exit = ref [||] in
    let transitions = ref [||] in
    let invocations = ref [||] in
    List.iter (fun child ->
      match translate_child child ancestors with
      | State idx -> append_mut_array idxs [|idx|];
      | OnEntry e -> append_mut_array on_enter e;
      | OnExit e -> append_mut_array on_exit e;
      | Transition t -> append_mut_array transitions [|t|];
      | Invoke i -> append_mut_array invocations [|i|];
      | Pass -> ()
    ) children;
    !idxs, !on_enter, !on_exit, !transitions, !invocations

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

  let state_map = !state_map in
  let states = map_to_array !states (fun s ->
    s
  ) in
  let transition_targets = !transition_targets in
  let transitions = map_to_array !transitions (fun t ->
    let targets = IntMap.find t.Tgt.Transition.idx transition_targets in
    let targets = List.map (fun k -> StringMap.find k state_map) targets in
    {t with Tgt.Transition.targets=Array.of_list targets}
  ) in

  states, transitions

let translate document =
  let states, transitions = flatten document in
  {
    Tgt.Document.name=document.Src.Document.name;
    states=states;
    transitions=transitions;
  }
