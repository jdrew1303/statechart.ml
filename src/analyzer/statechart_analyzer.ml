(*
STEPS

States:
* get the binding - default to early
* if the binding is early move all of the data elements to the first state
* assign an index to each state
* promote onentry, onexit, and invoke to state
* promote script tag to onentry
* promote donedata to state
* if the binding is late promote datamodel to state
* assign the state type:
  INITIAL | FINAL | HISTORY_DEEP | HISTORY_SHALLOW | ATOMIC | PARALLEL | COMPOUND
* mark the state as having a history child or not
* establish states ancestors

Transitions:
* compute the exit set
* compute the conflict set
* compute the transition target set
* compute the transition source
* compute the transition type:
  TARGETLESS | INTERNAL | SPONTANEOUS | HISTORY | INITIAL
* promote executable content to on_transition
*)

open Statechart_analyzer_types
open Statechart_datamodel
module SC = Statechart

let push_error errors baseline lm =
  let line, message = lm in
  errors := ((baseline + line - 1), message) :: !errors

let get_option opt default =
  match opt with
  | Some v -> v
  | None -> default

let map_id map id idx =
  match id with
  | Some id -> StateIDMap.add id idx map
  | _ -> map

let rec assign_idx_rec node idx tsn map ancestors =
  match node with
  | State s ->
    let map = map_id map s.State.id idx in
    let children, count, tsn, map, descendants =
      assign_children s.State.children (idx + 1) tsn map ancestors in
    State {s with
      State.idx=Some idx;
      children;
      ancestors;
      descendants
    }, count, tsn, map, idx :: descendants
  | Parallel s ->
    let map = map_id map s.Parallel.id idx in
    let children, count, tsn, map, descendants =
      assign_children s.Parallel.children (idx + 1) tsn map ancestors in
    Parallel {s with
      Parallel.idx=Some idx;
      children;
      ancestors;
      descendants
    }, count, tsn, map, idx :: descendants
  | Transition s ->
    Transition {s with
      Transition.idx=Some tsn;
      ancestors;
    }, idx, (tsn + 1), map, []
  | Initial s ->
    let children, count, tsn, map, descendants =
      assign_children s.Initial.children (idx + 1) tsn map ancestors in
    Initial {s with
      Initial.idx=Some idx;
      children;
      ancestors;
      descendants
    }, count, tsn, map, idx :: descendants
  | Final s ->
    let map = map_id map s.Final.id idx in
    let children, count, tsn, map, descendants =
      assign_children s.Final.children (idx + 1) tsn map ancestors in
    Final {s with
      Final.idx=Some idx;
      children;
      ancestors;
      descendants
    }, count, tsn, map, idx :: descendants
  | History s ->
    let map = map_id map s.History.id idx in
    let children, count, tsn, map, descendants =
      assign_children s.History.children (idx + 1) tsn map ancestors in
    History {s with
      History.idx=Some idx;
      children;
      ancestors;
      descendants
    }, count, tsn, map, idx :: descendants
  | _ -> node, idx, tsn, map, []

and assign_children children parent tsn map ancs =
  let ancs = parent :: ancs in
  let c, idx, tsn, map, desc = List.fold_left (fun acc child ->
    let c, idx, tsn, map, desc = acc in
    let child, idx, tsn, map, c_desc = assign_idx_rec child idx tsn map ancs in
    let c = child :: c in
    c, idx, tsn, map, (List.append desc c_desc)
  ) ([], parent, tsn, map, []) children in
  (List.rev c), idx, tsn, map, desc

let assign_idx doc =
  let map = StateIDMap.empty in
  let children, count, _tsn, map, _desc = assign_children doc.Document.children 0 0 map [] in
  {doc with Document.children; state_map=Some map; state_count=count}

let select_datamodel doc datamodels =
  let line = get_option doc.Document.line 0 in
  match doc.Document.data_model with
  | None ->
    fun _ -> Error [(line, "The datamodel attribute was not specified")]
  | Some dm ->
    try
      DatamodelMap.find dm datamodels
    with
    | _ ->
      fun _ -> Error [(line, "Unsupported datamodel: " ^ dm)]

let parse_expr dm errors line expr =
  match expr with
  | Expr s -> (
    match dm s with
    | Program expr -> ExprParsed expr
    | Error errs -> (
      List.iter (push_error errors line) errs;
      (* TODO make this a runtime exception instead of unset *)
      ExprUnset
    )
  )
  | _ -> expr

let rec parse_child dm errors child =
  match child with
  | State s ->
    let children = parse_children dm errors s.State.children in
    State {s with State.children}
  | Parallel s ->
    let children = parse_children dm errors s.Parallel.children in
    Parallel {s with Parallel.children}
  | Transition s ->
    let line = get_option s.Transition.line 1 in
    let cond = parse_expr dm errors line s.Transition.cond in
    let children = parse_children dm errors s.Transition.children in
    Transition {s with Transition.children; cond}
  | Initial s ->
    let children = parse_children dm errors s.Initial.children in
    Initial {s with Initial.children}
  | Final s ->
    let children = parse_children dm errors s.Final.children in
    Final {s with Final.children}
  | OnEntry s ->
    let children = parse_children dm errors s.OnEntry.children in
    OnEntry {s with OnEntry.children}
  | OnExit s ->
    let children = parse_children dm errors s.OnExit.children in
    OnExit {s with OnExit.children}
  | History s ->
    let children = parse_children dm errors s.History.children in
    History {s with History.children}
  | Case s ->
    let children = List.map (fun clause ->
      let line = get_option clause.CaseClause.line 1 in
      let cond = parse_expr dm errors line clause.CaseClause.cond in
      let children = parse_children dm errors clause.CaseClause.children in
      {clause with CaseClause.children; cond}
    ) s.Case.children in
    Case {s with Case.children}
  | Foreach s ->
    let line = get_option s.Foreach.line 1 in
    let array = parse_expr dm errors line s.Foreach.array in
    let item = parse_expr dm errors line s.Foreach.item in
    let index = parse_expr dm errors line s.Foreach.index in
    let children = parse_children dm errors s.Foreach.children in
    Foreach {s with Foreach.children; array; item; index}
  | Log s ->
    let line = get_option s.Log.line 1 in
    let expr = parse_expr dm errors line s.Log.expr in
    Log {s with Log.expr}
  | DataModel s ->
    let children = parse_children dm errors s.DataModel.children in
    DataModel {s with DataModel.children}
  | Data s ->
    let line = get_option s.Data.line 1 in
    let expr = parse_expr dm errors line s.Data.expr in
    (* TODO id? *)
    (* TODO src *)
    (* TODO *)
    let children = s.Data.children in
    Data {s with Data.children; expr}
  | Assign s ->
    let line = get_option s.Assign.line 1 in
    let location = parse_expr dm errors line s.Assign.location in
    let expr = parse_expr dm errors line s.Assign.expr in
    (* TODO *)
    let children = s.Assign.children in
    Assign {s with Assign.children; location; expr}
  | DoneData s ->
    let children = parse_children dm errors s.DoneData.children in
    DoneData {s with DoneData.children}
  | Content s ->
    let line = get_option s.Content.line 1 in
    let expr = parse_expr dm errors line s.Content.expr in
    (* TODO *)
    let children = s.Content.children in
    Content {s with Content.children; expr}
  | Param s ->
    let line = get_option s.Param.line 1 in
    let location = parse_expr dm errors line s.Param.location in
    let expr = parse_expr dm errors line s.Param.expr in
    Param {s with Param.location; expr}
  (* TODO script *)
  | Send s ->
    let line = get_option s.Send.line 1 in
    let event = parse_expr dm errors line s.Send.event in
    let target = parse_expr dm errors line s.Send.target in
    let t = parse_expr dm errors line s.Send.t in
    let id = parse_expr dm errors line s.Send.id in
    let delay = parse_expr dm errors line s.Send.delay in
    let namelist = List.map (parse_expr dm errors line) s.Send.namelist in
    let children = parse_children dm errors s.Send.children in
    Send {s with Send.event; target; t; id; delay; namelist; children}
  | Cancel s ->
    let line = get_option s.Cancel.line 1 in
    let sendid = parse_expr dm errors line s.Cancel.sendid in
    Cancel {s with Cancel.sendid}
  | Invoke s ->
    let line = get_option s.Invoke.line 1 in
    let t = parse_expr dm errors line s.Invoke.t in
    let src = parse_expr dm errors line s.Invoke.src in
    let id = parse_expr dm errors line s.Invoke.id in
    let namelist = List.map (parse_expr dm errors line) s.Invoke.namelist in
    let children = parse_children dm errors s.Invoke.children in
    Invoke {s with Invoke.t; src; id; namelist; children}
  | Finalize s ->
    let children = parse_children dm errors s.Finalize.children in
    Finalize {s with Finalize.children}
  | _ -> child

and parse_children dm errors children =
  List.map (parse_child dm errors) children

let parse_exprs dm errors doc =
  let children = parse_children dm errors doc.Document.children in
  {doc with Document.children}

exception ParseError of string
let analyze doc datamodels =
  let errors = ref [] in
  let doc = assign_idx doc in
  let dm = select_datamodel doc datamodels in
  let doc = parse_exprs dm errors doc in
  doc
