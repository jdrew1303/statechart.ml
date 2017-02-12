open Statechart_t

type result = Program of Statechart_executable.expression
            | Error of (loc * string) list

type parser = string -> result

let default_loc = ((0, 0), (0, 0))

let shift_loc base loc =
  let line, col = base in
  let l, c = loc in
  line + l - 1, col + c - 1

let push_error errors baseloc lm =
  let start, finish = baseloc in
  let (s, f), message = lm in
  let loc = (shift_loc start s), (shift_loc finish f) in
  errors := (loc, message) :: !errors

let get_option opt default =
  match opt with
  | Some v -> v
  | None -> default

let select_datamodel doc datamodels =
  let loc = get_option doc.Document.loc default_loc in
  match doc.Document.datamodel with
  | None ->
    fun _ -> Error [(loc, "The datamodel attribute was not specified")]
  | Some dm ->
    let rec find i =
      if i < 0 then fun _ -> Error [(loc, "Unsupported datamodel: " ^ dm)]
      else (
        match Array.get datamodels i with
        | n, parser when n == dm -> parser
        | _ -> find (i - 1)
      )
    in
    find ((Array.length datamodels) - 1)

let parse_expr dm errors loc expr =
  match expr with
  | Expr s -> (
    match dm s with
    | Program expr -> ExprParsed expr
    | Error errs -> (
      List.iter (push_error errors loc) errs;
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
    let loc = get_option s.Transition.loc default_loc in
    let cond = parse_expr dm errors loc s.Transition.cond in
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
      let loc = get_option clause.CaseClause.loc default_loc in
      let cond = parse_expr dm errors loc clause.CaseClause.cond in
      let children = parse_children dm errors clause.CaseClause.children in
      {clause with CaseClause.children; cond}
    ) s.Case.children in
    Case {s with Case.children}
  | Foreach s ->
    let loc = get_option s.Foreach.loc default_loc in
    let array = parse_expr dm errors loc s.Foreach.array in
    let item = parse_expr dm errors loc s.Foreach.item in
    let index = parse_expr dm errors loc s.Foreach.index in
    let children = parse_children dm errors s.Foreach.children in
    Foreach {s with Foreach.children; array; item; index}
  | Log s ->
    let loc = get_option s.Log.loc default_loc in
    let expr = parse_expr dm errors loc s.Log.expr in
    Log {s with Log.expr}
  | DataModel s ->
    let children = parse_children dm errors s.DataModel.children in
    DataModel {s with DataModel.children}
  | Data s ->
    let loc = get_option s.Data.loc default_loc in
    let expr = parse_expr dm errors loc s.Data.expr in
    (* TODO id? *)
    (* TODO src *)
    (* TODO *)
    let children = s.Data.children in
    Data {s with Data.children; expr}
  | Assign s ->
    let loc = get_option s.Assign.loc default_loc in
    let location = parse_expr dm errors loc s.Assign.location in
    let expr = parse_expr dm errors loc s.Assign.expr in
    (* TODO *)
    let children = s.Assign.children in
    Assign {s with Assign.children; location; expr}
  | DoneData s ->
    let children = parse_children dm errors s.DoneData.children in
    DoneData {s with DoneData.children}
  | Content s ->
    let loc = get_option s.Content.loc default_loc in
    let expr = parse_expr dm errors loc s.Content.expr in
    (* TODO *)
    let children = s.Content.children in
    Content {s with Content.children; expr}
  | Param s ->
    let loc = get_option s.Param.loc default_loc in
    let location = parse_expr dm errors loc s.Param.location in
    let expr = parse_expr dm errors loc s.Param.expr in
    Param {s with Param.location; expr}
  (* TODO script *)
  | Send s ->
    let loc = get_option s.Send.loc default_loc in
    let event = parse_expr dm errors loc s.Send.event in
    let target = parse_expr dm errors loc s.Send.target in
    let t = parse_expr dm errors loc s.Send.t in
    let id = parse_expr dm errors loc s.Send.id in
    let delay = parse_expr dm errors loc s.Send.delay in
    let namelist = List.map (parse_expr dm errors loc) s.Send.namelist in
    let children = parse_children dm errors s.Send.children in
    Send {s with Send.event; target; t; id; delay; namelist; children}
  | Cancel s ->
    let loc = get_option s.Cancel.loc default_loc in
    let sendid = parse_expr dm errors loc s.Cancel.sendid in
    Cancel {s with Cancel.sendid}
  | Invoke s ->
    let loc = get_option s.Invoke.loc default_loc in
    let t = parse_expr dm errors loc s.Invoke.t in
    let src = parse_expr dm errors loc s.Invoke.src in
    let id = parse_expr dm errors loc s.Invoke.id in
    let namelist = List.map (parse_expr dm errors loc) s.Invoke.namelist in
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
let parse doc datamodels =
  let errors = ref [] in
  let dm = select_datamodel doc datamodels in
  let doc = parse_exprs dm errors doc in
  doc, List.rev !errors
