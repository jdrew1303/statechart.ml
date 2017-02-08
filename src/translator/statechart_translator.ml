module Src = Statechart_analyzer_types
module Tgt = Statechart

type child_type = State of int
                | OnEntry of Tgt.expression list
                | OnExit of Tgt.expression list
                | Transition of Tgt.transition
                | Invoke of Tgt.invoke
                | Pass

exception MissingAnalysis
let unwrap maybe =
  match maybe with
  | Some value -> value
  | None -> raise MissingAnalysis

exception UnparsedExpression of string
let unwrap_expr expr =
  match expr with
  | Src.Expr v -> raise (UnparsedExpression v)
  | Src.ExprValue v -> Tgt.string_expr v
  | Src.ExprParsed v -> v
  | Src.ExprUnset -> Tgt.null

let get_parent ancestors =
  match ancestors with
  | parent :: _ -> Some parent
  | _ -> None

exception UnresolvableState of string
let resolve_list l idmap =
  List.map (fun key ->
    try
      Src.StateIDMap.find key idmap
    with
    | _ -> (
      raise (UnresolvableState key)
    )
  ) l

let empty_state =
  {
    Tgt.State.idx=0;
    depth=0;
    id=None;
    t=`basic;
    initial=[];
    transitions=[];
    invocations=[];
    on_enter=[];
    on_exit=[];
    children=[];
    parent=None;
    ancestors=[];
    descendants=[];
    history=[];
    history_type=None;
  }

let rec translate_state states idmap state =
  let idx = unwrap state.Src.State.idx in
  let children, on_enter, on_exit, trans, invs =
    translate_children states idmap state.Src.State.children in
  let ancestors = state.Src.State.ancestors in
  Array.set states idx {
    Tgt.State.idx=idx;
    depth=List.length ancestors;
    id=state.Src.State.id;
    t=(match children with
    | [] -> `basic
    | _ -> `compound);
    initial=resolve_list state.Src.State.initial idmap;
    transitions=trans;
    invocations=invs;
    on_enter=on_enter;
    on_exit=on_exit;
    children=children;
    parent=get_parent ancestors;
    ancestors=ancestors;
    descendants=state.Src.State.descendants;
    history=[];
    history_type=None;
  };
  idx

and translate_parallel states idmap state =
  let idx = unwrap state.Src.Parallel.idx in
  let children, on_enter, on_exit, trans, invs =
    translate_children states idmap state.Src.Parallel.children in
  let ancestors = state.Src.Parallel.ancestors in
  Array.set states idx {
    Tgt.State.idx=idx;
    depth=List.length ancestors;
    id=state.Src.Parallel.id;
    t=`parallel;
    initial=[];
    transitions=trans;
    invocations=invs;
    on_enter=on_enter;
    on_exit=on_exit;
    children=children;
    parent=get_parent ancestors;
    ancestors=ancestors;
    descendants=state.Src.Parallel.descendants;
    history=[];
    history_type=None;
  };
  idx

and translate_initial states idmap state =
  let idx = unwrap state.Src.Initial.idx in
  let children, on_enter, on_exit, trans, invs =
    translate_children states idmap state.Src.Initial.children in
  let ancestors = state.Src.Initial.ancestors in
  Array.set states idx {
    Tgt.State.idx=idx;
    depth=List.length ancestors;
    id=None;
    t=`initial;
    initial=[];
    transitions=trans;
    invocations=invs;
    on_enter=on_enter;
    on_exit=on_exit;
    children=children;
    parent=get_parent ancestors;
    ancestors=ancestors;
    descendants=state.Src.Initial.descendants;
    history=[];
    history_type=None;
  };
  idx

and translate_final states idmap state =
  let idx = unwrap state.Src.Final.idx in
  let children, on_enter, on_exit, trans, invs =
    translate_children states idmap state.Src.Final.children in
  let ancestors = state.Src.Final.ancestors in
  Array.set states idx {
    Tgt.State.idx=idx;
    depth=List.length ancestors;
    id=state.Src.Final.id;
    t=`final;
    initial=[];
    transitions=trans;
    invocations=invs;
    on_enter=on_enter;
    on_exit=on_exit;
    children=children;
    parent=get_parent ancestors;
    ancestors=ancestors;
    descendants=state.Src.Final.descendants;
    history=[];
    history_type=None;
  };
  idx

and translate_history states idmap state =
  let idx = unwrap state.Src.History.idx in
  let children, on_enter, on_exit, trans, invs =
    translate_children states idmap state.Src.History.children in
  let ancestors = state.Src.History.ancestors in
  Array.set states idx {
    Tgt.State.idx=idx;
    depth=List.length ancestors;
    id=state.Src.History.id;
    t=`history;
    initial=[];
    transitions=trans;
    invocations=invs;
    on_enter=on_enter;
    on_exit=on_exit;
    children=children;
    parent=get_parent ancestors;
    ancestors=ancestors;
    descendants=state.Src.History.descendants;
    history=[]; (* TODO *)
    history_type=Some state.Src.History.t;
  };
  idx

and translate_transition states idmap transition =
  let idx = unwrap transition.Src.Transition.idx in
  let ancestors = transition.Src.Transition.ancestors in
  let parent = get_parent ancestors in
  let condition = match transition.Src.Transition.cond with
  | Src.ExprUnset -> None
  | cond -> Some (unwrap_expr cond) in
  let children = translate_executables transition.Src.Transition.children in
  {
    Tgt.Transition.idx=idx;
    depth=List.length ancestors;
    (* TODO *)
    scope=0;
    source=parent;
    targets=resolve_list transition.Src.Transition.target idmap;
    events=transition.Src.Transition.event;
    condition=condition;
    t=transition.Src.Transition.t;
    on_transition=children;
  }

and translate_child states idmap child =
  match child with
  | Src.State c -> State (translate_state states idmap c)
  | Src.Parallel c -> State (translate_parallel states idmap c)
  | Src.Transition t -> Transition (translate_transition states idmap t)
  | Src.Initial c -> State (translate_initial states idmap c)
  | Src.Final c -> State (translate_final states idmap c)
  | Src.OnEntry c -> OnEntry (translate_executables c.Src.OnEntry.children)
  | Src.OnExit c -> OnExit (translate_executables c.Src.OnExit.children)
  | Src.History c -> State (translate_history states idmap c)
  (* TODO datamodel *)
  (* TODO invoke *)
  | _ -> Pass

and translate_children states idmap children =
  let init = [], [], [], [], [] in
  List.fold_right (fun child acc ->
    let idxs, on_enter, on_exit, trans, invs = acc in
    match translate_child states idmap child with
    | State idx -> idx :: idxs, on_enter, on_exit, trans, invs
    | OnEntry e -> idxs, (List.append e on_enter), on_exit, trans, invs
    | OnExit e -> idxs, on_enter, (List.append e on_enter), trans, invs
    | Transition t -> idxs, on_enter, on_exit, t :: trans, invs
    | Invoke i -> idxs, on_enter, on_exit, trans, i :: invs
    | Pass -> acc
  ) children init

and translate_raise raise =
  let event = unwrap raise.Src.Raise.event in
  Tgt.complex_expr `raise [Tgt.string_expr event]

and translate_case case =
  let children = List.map translate_case_clause case.Src.Case.children in
  Tgt.complex_expr `case children

and translate_case_clause clause =
  Tgt.complex_expr `clause [
    unwrap_expr clause.Src.CaseClause.cond;
    Tgt.complex_expr `block (translate_executables clause.Src.CaseClause.children);
  ]

and translate_foreach foreach =
  Tgt.complex_expr `foreach [
    unwrap_expr foreach.Src.Foreach.array;
    unwrap_expr foreach.Src.Foreach.item;
    unwrap_expr foreach.Src.Foreach.index;
    Tgt.complex_expr `block (translate_executables foreach.Src.Foreach.children);
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
  | Src.Raise e -> [translate_raise e]
  | Src.Case e -> [translate_case e]
  | Src.Foreach e -> [translate_foreach e]
  | Src.Log e -> [translate_log e]
  (* TODO datamodel? *)
  | Src.Assign e -> [translate_assign e]
  (* TODO done data? *)
  (* TODO content? *)
  (* TODO param? *)
  | Src.Send e -> [translate_send e]
  | Src.Cancel e -> [translate_cancel e]
  (* TODO invoke? *)
  | Src.Finalize e -> translate_executables e.Src.Finalize.children
  | _ -> []

and translate_executables children =
  List.fold_right (fun child acc ->
    List.append (translate_executable child) acc
  ) children []

let translate_document_child states idmap child =
  let _idx = match child with
  | Src.State c -> translate_state states idmap c
  | Src.Parallel c -> translate_parallel states idmap c
  | Src.Final c -> translate_final states idmap c
  | Src.History c -> translate_history states idmap c
  (* TODO handle datamodel *)
  (* TODO handle script *)
  | _ -> -1 in
  ()

let translate_document_children document =
  let idmap = unwrap document.Src.Document.state_map in
  let states = Array.make document.Src.Document.state_count empty_state in
  List.iter (translate_document_child states idmap) document.Src.Document.children;
  states

let translate document =
  (* TODO handle initial *)
  let states = translate_document_children document in
  {
    Tgt.Document.name=document.Src.Document.name;
    initial_transitions=[];
    states=Array.to_list states;
  }
