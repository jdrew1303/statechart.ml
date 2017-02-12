open Statechart_t

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

let get_prop_expr_alias props name append =
  match get_prop props name with
  | Some value -> ExprValue value
  | None -> (
    match get_prop props (name ^ append) with
    | Some expr -> Expr expr
    | None -> ExprUnset
  )

let get_prop_expr props name =
  match get_prop props name with
  | Some expr -> Expr expr
  | None -> ExprUnset

let get_prop_bool props name =
  match get_prop props name with
  | Some "true" -> Some true
  | Some "TRUE" -> Some true
  | Some "True" -> Some true
  | Some _ -> Some false
  | _ -> None

let ws_re = Re.compile (Re_emacs.re "[ \t\n\r]+")
let ws_split str =
  let tokens = Re.split ws_re str in
  List.filter (fun s -> match s with
    | "" -> false
    | _ -> true
  ) tokens

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

let parse_scxml line props children =
  Document {
    Document.name=get_prop props "name";
    initial=get_prop props "initial" |> parse_string_list;
    datamodel=get_prop props "datamodel";
    binding=get_prop props "binding" |> parse_binding;
    children=children;
    line=line;
  }

let parse_state line props children =
  State {
    State.id=get_prop props "id";
    initial=get_prop props "initial" |> parse_string_list;
    children=children;
    line=line;
  }

let parse_parallel line props children =
  Parallel {
    Parallel.id=get_prop props "id";
    children=children;
    line=line;
  }

let parse_transition_type t =
  match t with
  | Some "internal" -> `internal
  | _ -> `external_

let parse_transition line props children =
  Transition {
    Transition.event=get_prop props "event" |> parse_string_list;
    cond=get_prop_expr props "cond";
    target=get_prop props "target" |> parse_string_list;
    t=get_prop props "type" |> parse_transition_type;
    children=children;
    line=line;
  }

let parse_initial line props children =
  Initial {
    Initial.children=children;
    line=line;
  }

let parse_final line props children =
  Final {
    Final.id=get_prop props "id";
    children=children;
    line=line;
  }

let parse_on_entry line props children =
  OnEntry {
    OnEntry.children=children;
    line=line;
  }

let parse_on_exit line props children =
  OnExit {
    OnExit.children=children;
    line=line;
  }

let parse_history_type t =
  match t with
  | Some "internal" -> `shallow
  | _ -> `deep

let parse_history line props children =
  History {
    History.id=get_prop props "id";
    t=get_prop props "type" |> parse_history_type;
    children=children;
    line=line;
  }

let parse_raise line props =
  Raise {
    Raise.event=get_prop props "event";
    line=line;
  }

let finish_if_child current prev cond line =
  match current with
  | [] -> ([], prev)
  | _ -> let c = {CaseClause.cond=cond; children=current; line=line} in
         ([], c :: prev)

let parse_if_child current prev child =
  match child with
  | CaseClause {CaseClause.cond=cond; line=line} -> finish_if_child current prev cond line
  | _ -> (child :: current, prev)

let rec parse_if_children current prev children =
  match children with
  | [] -> current, prev
  | child :: rest ->
    let current, prev = parse_if_child current prev child in
    parse_if_children current prev rest

let parse_if line props children =
  let rev_children = List.rev children in
  let current, prev = parse_if_children [] [] rev_children in
  let cond = get_prop_expr props "cond" in
  let _, children = finish_if_child current prev cond line in
  Case {
    Case.children=children;
    line=line;
  }

let parse_if_else line props =
  CaseClause {
    CaseClause.cond=get_prop_expr props "cond";
    children=[];
    line=line;
  }

let parse_else line =
  CaseClause {
    CaseClause.cond=ExprUnset;
    children=[];
    line=line;
  }

let parse_foreach line props children =
  Foreach {
    Foreach.array=get_prop_expr props "array";
    item=get_prop_expr props "item";
    index=get_prop_expr props "index";
    children=children;
    line=line;
  }

let parse_log line props children =
  Log {
    Log.expr=get_prop_expr props "expr";
    label=get_prop props "label";
    line=line;
  }

let parse_data_model line children =
  DataModel {
    DataModel.children=children;
    line=line;
  }

let parse_data line props children =
  Data {
    Data.id=get_prop props "id";
    src=get_prop props "src";
    expr=get_prop_expr props "expr";
    (* TODO *)
    children=Some "";
    line=line;
  }

let parse_assign line props children =
  Assign {
    Assign.location=get_prop_expr props "location";
    expr=get_prop_expr props "expr";
    (* TODO *)
    children=Some "";
    line=line;
  }

let parse_done_data line children =
  DoneData {
    DoneData.children=children;
    line=line;
  }

let parse_content line props children =
  Content {
    Content.expr=get_prop_expr props "expr";
    (* TODO *)
    children=Some "";
    line=line;
  }

let parse_param line props =
  Param {
    Param.name=get_prop props "name";
    expr=get_prop_expr props "expr";
    location=get_prop_expr props "location";
    line=line;
  }

let parse_script line props children =
  Script {
    Script.src=get_prop props "src";
    (* TODO *)
    children=Some "";
    line=line;
  }

let parse_send line props children =
  Send {
    Send.event=get_prop_expr_alias props "event" "expr";
    target=get_prop_expr_alias props "target" "expr";
    t=get_prop_expr_alias props "type" "expr";
    id=get_prop_expr_alias props "id" "location";
    delay=get_prop_expr_alias props "delay" "expr";
    namelist=get_prop props "namelist" |> parse_string_list |> (List.map (fun s -> Expr s));
    children=children;
    line=line;
  }

let parse_cancel line props =
  Cancel {
    Cancel.sendid=get_prop_expr_alias props "sendid" "expr";
    line=line;
  }

let parse_invoke line props children =
  Invoke {
    Invoke.t=get_prop_expr_alias props "type" "expr";
    src=get_prop_expr_alias props "src" "expr";
    id=get_prop_expr_alias props "id" "location";
    namelist=get_prop props "namelist" |> parse_string_list |> (List.map (fun s -> Expr s));
    autoforward=get_prop_bool props "autoforward";
    children=children;
    line=line;
  }

let parse_finalize line children =
  Finalize {
    Finalize.children=children;
    line=line;
  }

let parse_element line name proplist children =
  let props = Props.of_list proplist in
  let ns, el = name in
  match ns with
  | "http://www.w3.org/2005/07/scxml" -> (
    match el with
    | "scxml" -> SCEl (parse_scxml line props (filter_children children))
    | "state" -> SCEl (parse_state line props (filter_children children))
    | "parallel" -> SCEl (parse_parallel line props (filter_children children))
    | "transition" -> SCEl (parse_transition line props (filter_children children))
    | "initial" -> SCEl (parse_initial  line props (filter_children children))
    | "final" -> SCEl (parse_final line props (filter_children children))
    | "onentry" -> SCEl (parse_on_entry line props (filter_children children))
    | "onexit" -> SCEl (parse_on_exit line props (filter_children children))
    | "history" -> SCEl (parse_history line props (filter_children children))
    | "raise" -> SCEl (parse_raise line props)
    | "if" -> SCEl (parse_if line props (filter_children children))
    | "ifelse" -> SCEl (parse_if_else line props)
    | "else" -> SCEl (parse_else line)
    | "foreach" -> SCEl (parse_foreach line props (filter_children children))
    | "log" -> SCEl (parse_log line props (filter_children children))
    | "datamodel" -> SCEl (parse_data_model line (filter_children children))
    | "data" -> SCEl (parse_data line props (filter_children children))
    | "assign" -> SCEl (parse_assign line props (filter_children children))
    | "donedata" -> SCEl (parse_done_data line (filter_children children))
    | "content" -> SCEl (parse_content line props children)
    | "param" -> SCEl (parse_param line props)
    | "script" -> SCEl (parse_script line props children)
    | "send" -> SCEl (parse_send line props (filter_children children))
    | "cancel" -> SCEl (parse_cancel line props)
    | "invoke" -> SCEl (parse_invoke line props (filter_children children))
    | "finalize" -> SCEl (parse_finalize line (filter_children children))
    | _ -> None
  )
  | _ -> None

let unwrap root =
  match root with
  | Some (SCEl (Document doc)) -> Some doc
  | _ -> None

let from_signals signals =
  signals
  |> Markup.tree
    ~text:(fun ss -> Text (String.concat "" ss))
    ~element:(parse_element None)
  |> unwrap

let from_stream stream =
  stream
  |> Markup.parse_xml
    ~encoding:Markup.Encoding.utf_8
  |> Markup.signals
  |> Markup.drain;
  (* |> tree
    ~text:(fun ss -> Text (String.concat "" ss))
    ~element:(fun n p c ->
      let line, _col = Markup.location parser in
      parse_element (Some line) n p c
    )
  |> unwrap *)
  Some {
    Document.name=Some "foo";
    initial=[];
    datamodel=None;
    binding=`early;
    children=[];
    line=None;
  }

let from_string str =
  from_stream (Markup.string str)

let from_buffer buffer =
  from_stream (Markup.buffer buffer)

let from_channel chn =
  from_stream (Markup.channel chn)
