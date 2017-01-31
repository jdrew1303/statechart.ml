open Markup
open Statechart_analyzer_types

module Prop = struct
  type t = Markup.name * string
  let compare a b =
    let (a_n, _) = a in
    let (b_n, _) = b in
    Pervasives.compare a_n b_n
end
module Props = Set.Make(Prop)

type html = Text of string
          | SCEl of statechart_el
          | None

let get_ns_prop props ns name =
  try
    let (_, value) = (Props.find ((ns, name), "") props) in
    Some value
  with
  | Not_found -> None

let get_prop props name =
  get_ns_prop props "" name

let get_prop_expr props name =
  match get_prop props name with
  | Some value -> ExprValue value
  | None -> (
    match get_prop props (name ^ "expr") with
    | Some expr -> Expr expr
    | None -> ExprUnset
  )

let get_prop_bool props name =
  match get_prop props name with
  | Some "true" -> Some true
  | Some "TRUE" -> Some true
  | Some "True" -> Some true
  | Some _ -> Some false
  | _ -> None

let split_on_chars = Core_kernel.Core_string.split_on_chars

let ws_split str =
  let tokens = split_on_chars str ~on: [' '; '\n'; '\t'] in
  List.filter (fun s -> String.equal s "" |> not) tokens

let map_children children =
List.map (fun child ->
  match child with
  | SCEl el -> el
  | _ -> Other
) children

let filter_children children =
let children = map_children children in
List.filter (fun child ->
  match child with
  | Other -> false
  | _ -> true
) children

let parse_binding binding =
  match binding with
  | Some "late" -> `late
  | _ -> `early

let parse_string_list str =
  match str with
  | Some s -> ws_split s
  | _ -> []

let parse_scxml props children =
  Document {
    Document.name=get_prop props "name";
    initial=get_prop props "initial" |> parse_string_list;
    data_model=get_prop props "datamodel";
    binding=get_prop props "binding" |> parse_binding;
    children=children;
  }

let parse_state props children =
  State {
    State.id=get_prop props "id";
    initial=get_prop props "initial" |> parse_string_list;
    children=children;
  }

let parse_parallel props children =
  Parallel {
    Parallel.id=get_prop props "id";
    children=children;
  }

let parse_transition_type t =
  match t with
  | Some "internal" -> `internal
  | _ -> `external_

let parse_transition props children =
  Transition {
    Transition.event=get_prop props "event" |> parse_string_list;
    cond=get_prop props "cond";
    target=get_prop props "target" |> parse_string_list;
    t=get_prop props "type" |> parse_transition_type;
    children=children;
  }

let parse_initial props children =
  Initial {
    Initial.children=children;
  }

let parse_final props children =
  Final {
    Final.id=get_prop props "id";
    children=children;
  }

let parse_on_entry props children =
  OnEntry {
    OnEntry.children=children;
  }

let parse_on_exit props children =
  OnExit {
    OnExit.children=children;
  }

let parse_history_type t =
  match t with
  | Some "internal" -> `shallow
  | _ -> `deep

let parse_history props children =
  History {
    History.id=get_prop props "id";
    t=get_prop props "type" |> parse_history_type;
    children=children;
  }

let parse_raise props =
  Raise {
    Raise.event=get_prop props "event";
  }

let finish_if_child current prev cond =
  match current with
  | [] -> ([], prev)
  | _ -> let c = {CaseClause.cond=cond; children=current} in
         ([], c :: prev)

let parse_if_child current prev child =
  match child with
  | CaseClause {CaseClause.cond=cond} -> finish_if_child current prev cond
  | _ -> (child :: current, prev)

let rec parse_if_children current prev children =
  match children with
  | [] -> current, prev
  | child :: rest ->
    let current, prev = parse_if_child current prev child in
    parse_if_children current prev rest

let parse_if props children =
  let rev_children = List.rev children in
  let current, prev = parse_if_children [] [] rev_children in
  let cond = get_prop props "cond" in
  let _, children = finish_if_child current prev cond in
  Case {
    Case.children=children;
  }

let parse_if_else props =
  CaseClause {
    CaseClause.cond=get_prop props "cond";
    children=[];
  }

let parse_else =
  CaseClause {
    CaseClause.cond=None;
    children=[];
  }

let parse_foreach props children =
  Foreach {
    Foreach.array=get_prop props "array";
    item=get_prop props "item";
    index=get_prop props "index";
    children=children;
  }

let parse_log props children =
  Log {
    Log.expr=get_prop props "expr";
    label=get_prop props "label";
  }

let parse_data_model children =
  DataModel {
    DataModel.children=children;
  }

let parse_data props children =
  Data {
    Data.id=get_prop props "id";
    src=get_prop props "src";
    expr=get_prop props "expr";
    (* TODO *)
    children=Some "";
  }

let parse_assign props children =
  Assign {
    Assign.location=get_prop props "location";
    expr=get_prop props "expr";
    (* TODO *)
    children=Some "";
  }

let parse_done_data children =
  DoneData {
    DoneData.children=children;
  }

let parse_content props children =
  Content {
    Content.expr=get_prop props "expr";
    (* TODO *)
    children=Some "";
  }

let parse_param props =
  Param {
    Param.name=get_prop props "name";
    expr=get_prop props "expr";
    location=get_prop props "location";
  }

let parse_script props children =
  Script {
    Script.src=get_prop props "src";
    (* TODO *)
    children=Some "";
  }

let parse_send props children =
  Send {
    Send.event=get_prop_expr props "event";
    target=get_prop_expr props "target";
    t=get_prop_expr props "type";
    id=get_prop_expr props "id";
    delay=get_prop_expr props "delay";
    namelist=get_prop props "namelist" |> parse_string_list;
    children=children;
  }

let parse_cancel props =
  Cancel {
    Cancel.sendid=get_prop_expr props "sendid";
  }

let parse_invoke props children =
  Invoke {
    Invoke.t=get_prop_expr props "type";
    src=get_prop_expr props "src";
    id=get_prop_expr props "id";
    namelist=get_prop props "namelist" |> parse_string_list;
    autoforward=get_prop_bool props "autoforward";
    children=children;
  }

let parse_finalize children =
  Finalize {
    Finalize.children=children;
  }

let parse_element name proplist children =
  let props = Props.of_list proplist in
  let ns, el = name in
  match ns with
  | "http://www.w3.org/2005/07/scxml" -> (
    match el with
    | "scxml" -> SCEl (parse_scxml props (filter_children children))
    | "state" -> SCEl (parse_state props (filter_children children))
    | "parallel" -> SCEl (parse_parallel props (filter_children children))
    | "transition" -> SCEl (parse_transition props (filter_children children))
    | "initial" -> SCEl (parse_initial props (filter_children children))
    | "final" -> SCEl (parse_final props (filter_children children))
    | "onentry" -> SCEl (parse_on_entry props (filter_children children))
    | "onexit" -> SCEl (parse_on_exit props (filter_children children))
    | "history" -> SCEl (parse_history props (filter_children children))
    | "raise" -> SCEl (parse_raise props)
    | "if" -> SCEl (parse_if props (filter_children children))
    | "ifelse" -> SCEl (parse_if_else props)
    | "else" -> SCEl parse_else
    | "foreach" -> SCEl (parse_foreach props (filter_children children))
    | "log" -> SCEl (parse_log props (filter_children children))
    | "datamodel" -> SCEl (parse_data_model (filter_children children))
    | "data" -> SCEl (parse_data props (filter_children children))
    | "assign" -> SCEl (parse_assign props (filter_children children))
    | "donedata" -> SCEl (parse_done_data (filter_children children))
    | "content" -> SCEl (parse_content props children)
    | "param" -> SCEl (parse_param props)
    | "script" -> SCEl (parse_script props children)
    | "send" -> SCEl (parse_send props (filter_children children))
    | "cancel" -> SCEl (parse_cancel props)
    | "invoke" -> SCEl (parse_invoke props (filter_children children))
    | "finalize" -> SCEl (parse_finalize (filter_children children))
    | _ -> None
  )
  | _ -> None

let unwrap root =
  match root with
  | Some (SCEl (Document doc)) -> Some doc
  | _ -> None

let from_signals signals =
  signals
  |> tree
    ~text:(fun ss -> Text (String.concat "" ss))
    ~element:(parse_element)
  |> unwrap

let from_stream stream =
  stream
  |> parse_xml
  |> Markup.signals
  |> from_signals

let from_string str = from_stream (string str)

let from_channel chn = from_stream (channel chn)
