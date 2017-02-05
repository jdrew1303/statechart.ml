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

let rec assign_idx_rec node idx map ancestors =
  match node with
  | State s ->
    let s = {s with State.idx=Some idx; ancestors} in
    let map = map_id map s.State.id idx in
    let children, idx, map = assign_children s.State.children idx map ancestors in
    State {s with State.children}, idx, map
  | Parallel s ->
    let s = {s with Parallel.idx=Some idx; ancestors} in
    let map = map_id map s.Parallel.id idx in
    let children, idx, map = assign_children s.Parallel.children idx map ancestors in
    Parallel {s with Parallel.children}, idx, map
  | Initial s ->
    let s = {s with Initial.idx=Some idx; ancestors} in
    let children, idx, map = assign_children s.Initial.children idx map ancestors in
    Initial {s with Initial.children}, idx, map
  | Final s ->
    let s = {s with Final.idx=Some idx; ancestors} in
    let map = map_id map s.Final.id idx in
    let children, idx, map = assign_children s.Final.children idx map ancestors in
    Final {s with Final.children}, idx, map
  | History s ->
    let s = {s with History.idx=Some idx; ancestors} in
    let map = map_id map s.History.id idx in
    let children, idx, map = assign_children s.History.children idx map ancestors in
    History {s with History.children}, idx, map
  | _ -> node, idx, map

and assign_children children parent map ancs =
  let ancs = parent :: ancs in
  let idx = parent + 1 in
  let c, idx, map = List.fold_left (fun acc child ->
    let c, idx, map = acc in
    let child, idx, map = assign_idx_rec child idx map ancs in
    let c = child :: c in
    c, (idx + 1), map
  ) ([], idx, map) children in
  (List.rev c), idx, map

let assign_idx doc =
  let map = StateIDMap.empty in
  let children, count, map = assign_children doc.Document.children (-1) map [] in
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
    (* TODO namelist *)
    let children = parse_children dm errors s.Send.children in
    Send {s with Send.event; target; t; id; delay; children}
  | Cancel s ->
    let line = get_option s.Cancel.line 1 in
    let sendid = parse_expr dm errors line s.Cancel.sendid in
    Cancel {s with Cancel.sendid}
  | Invoke s ->
    let line = get_option s.Invoke.line 1 in
    let t = parse_expr dm errors line s.Invoke.t in
    let src = parse_expr dm errors line s.Invoke.src in
    let id = parse_expr dm errors line s.Invoke.id in
    (* TODO namelist *)
    let children = parse_children dm errors s.Invoke.children in
    Invoke {s with Invoke.t; src; id; children}
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
