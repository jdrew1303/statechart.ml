module Src = Statechart_analyzer_types
module Tgt = Statechart
open Statechart_datamodel

exception MissingAnalysis

type child_type = State of int
                | OnEnter of Tgt.expression
                | OnExit of Tgt.expression
                | Transition of Tgt.transition
                | Invoke of Tgt.invoke
                | None

(* TODO throw an exception *)
let unwrap maybe =
  match maybe with
  | Some value -> value
  | None -> raise MissingAnalysis

let get_parent ancestors =
  match ancestors with
  | parent :: _ -> Some parent
  | _ -> None

let resolve_list l idmap =
  List.map (fun key -> Src.StateIDMap.find key idmap) l

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
    history=None;
    history_type=None;
  }

let rec translate_state states idmap data_model state =
  let idx = unwrap state.Src.State.idx in
  let children, on_enter, on_exit, trans, invs =
    translate_children states idmap data_model state.Src.State.children in
  let ancestors = state.Src.State.ancestors in
  Array.set states idx {
    Tgt.State.idx=idx;
    depth=List.length ancestors;
    id=state.Src.State.id;
    t=(match children with
    | [] -> `basic
    | _ -> `composite);
    initial=resolve_list state.Src.State.initial idmap;
    transitions=trans;
    invocations=invs;
    on_enter=on_enter;
    on_exit=on_exit;
    children=children;
    parent=get_parent ancestors;
    ancestors=ancestors;
    descendants=state.Src.State.descendants;
    history=None;
    history_type=None;
  };
  idx

and translate_parallel states idmap data_model state =
  let idx = unwrap state.Src.Parallel.idx in
  let children, on_enter, on_exit, trans, invs =
    translate_children states idmap data_model state.Src.Parallel.children in
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
    history=None;
    history_type=None;
  };
  idx

and translate_initial states idmap data_model state =
  let idx = unwrap state.Src.Initial.idx in
  let children, on_enter, on_exit, trans, invs =
    translate_children states idmap data_model state.Src.Initial.children in
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
    history=None;
    history_type=None;
  };
  idx

and translate_final states idmap data_model state =
  let idx = unwrap state.Src.Final.idx in
  let children, on_enter, on_exit, trans, invs =
    translate_children states idmap data_model state.Src.Final.children in
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
    history=None;
    history_type=None;
  };
  idx

and translate_history states idmap data_model state =
  let idx = unwrap state.Src.History.idx in
  let children, on_enter, on_exit, trans, invs =
    translate_children states idmap data_model state.Src.History.children in
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
    history=None; (* TODO *)
    history_type=Some state.Src.History.t;
  };
  idx

and translate_child states idmap data_model child =
  match child with
  | Src.State c -> State (translate_state states idmap data_model c)
  | Src.Parallel c -> State (translate_parallel states idmap data_model c)
  | Src.Initial c -> State (translate_initial states idmap data_model c)
  | Src.Final c -> State (translate_final states idmap data_model c)
  | Src.History c -> State (translate_history states idmap data_model c)
  | _ -> None

and translate_children states idmap data_model children =
  let init = [], [], [], [], [] in
  List.fold_right (fun child acc ->
    let idxs, on_enter, on_exit, trans, invs = acc in
    match translate_child states idmap data_model child with
    | State idx -> idx :: idxs, on_enter, on_exit, trans, invs
    | OnEnter e -> idxs, e :: on_enter, on_exit, trans, invs
    | OnExit e -> idxs, on_enter, e :: on_exit, trans, invs
    | Transition t -> idxs, on_enter, on_exit, t :: trans, invs
    | Invoke i -> idxs, on_enter, on_exit, trans, i :: invs
    | None -> acc
  ) children init

let translate_document_child states idmap data_model child =
  let _idx = match child with
  | Src.State c -> translate_state states idmap data_model c
  | Src.Parallel c -> translate_parallel states idmap data_model c
  | Src.Final c -> translate_final states idmap data_model c
  | Src.History c -> translate_history states idmap data_model c
  (* TODO handle data_model *)
  (* TODO handle script *)
  | _ -> -1 in
  ()

let translate_document_children document data_model =
  let idmap = unwrap document.Src.Document.state_map in
  let states = Array.make document.Src.Document.state_count empty_state in
  List.iter (translate_document_child states idmap data_model) document.Src.Document.children;
  states

let select_data_model datamodels doc =
  match doc.Src.Document.data_model with
  | Some dm -> DatamodelMap.find dm datamodels
  | None -> DatamodelMap.find "null" datamodels

let translate document datamodels =
  (* TODO handle initial *)
  let data_model = select_data_model datamodels document in
  let states = translate_document_children document data_model in
  {
    Tgt.Document.name=document.Src.Document.name;
    initial_transitions=[];
    states=states;
  }
