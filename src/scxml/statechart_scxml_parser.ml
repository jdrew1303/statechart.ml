open Statechart_t

type prop_name = (string * string)
type prop = prop_name * string
type parse_string_list = string -> string list

module Prop = struct
  type t = prop
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

let map_children children =
  List.map (fun child ->
    match child with
    | SCEl el -> el
    | _ -> Other
  ) children

let parse_string_list split str =
  match str with
  | Some s -> split s
  | _ -> []

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

let parse_scxml parse_string_list loc props children =
  Document {
    Document.name=get_prop props "name";
    initial=get_prop props "initial" |> parse_string_list;
    datamodel=get_prop props "datamodel";
    binding=get_prop props "binding" |> parse_binding;
    children=children;
    loc=loc;
  }

let parse_state parse_string_list loc props children =
  State {
    State.id=get_prop props "id";
    initial=get_prop props "initial" |> parse_string_list;
    children=children;
    loc=loc;
  }

let parse_parallel loc props children =
  Parallel {
    Parallel.id=get_prop props "id";
    children=children;
    loc=loc;
  }

let parse_transition_type t =
  match t with
  | Some "internal" -> `internal
  | _ -> `external_

let parse_transition parse_string_list loc props children =
  Transition {
    Transition.event=get_prop props "event" |> parse_string_list;
    cond=get_prop_expr props "cond";
    target=get_prop props "target" |> parse_string_list;
    t=get_prop props "type" |> parse_transition_type;
    children=children;
    loc=loc;
  }

let parse_initial loc props children =
  Initial {
    Initial.children=children;
    loc=loc;
  }

let parse_final loc props children =
  Final {
    Final.id=get_prop props "id";
    children=children;
    loc=loc;
  }

let parse_on_entry loc props children =
  OnEntry {
    OnEntry.children=children;
    loc=loc;
  }

let parse_on_exit loc props children =
  OnExit {
    OnExit.children=children;
    loc=loc;
  }

let parse_history_type t =
  match t with
  | Some "deep" -> `deep
  | _ -> `shallow

let parse_history loc props children =
  History {
    History.id=get_prop props "id";
    t=get_prop props "type" |> parse_history_type;
    children=children;
    loc=loc;
  }

let parse_raise loc props =
  Raise {
    Raise.event=get_prop props "event";
    loc=loc;
  }

let finish_if_child current prev cond loc =
  match current with
  | [] -> ([], prev)
  | _ -> let c = {CaseClause.cond=cond; children=current; loc=loc} in
         ([], c :: prev)

let parse_if_child current prev child =
  match child with
  | CaseClause {CaseClause.cond=cond; loc=loc} -> finish_if_child current prev cond loc
  | _ -> (child :: current, prev)

let rec parse_if_children current prev children =
  match children with
  | [] -> current, prev
  | child :: rest ->
    let current, prev = parse_if_child current prev child in
    parse_if_children current prev rest

let parse_if loc props children =
  let rev_children = List.rev children in
  let current, prev = parse_if_children [] [] rev_children in
  let cond = get_prop_expr props "cond" in
  let _, children = finish_if_child current prev cond loc in
  Case {
    Case.children=children;
    loc=loc;
  }

let parse_if_else loc props =
  CaseClause {
    CaseClause.cond=get_prop_expr props "cond";
    children=[];
    loc=loc;
  }

let parse_else loc =
  CaseClause {
    CaseClause.cond=ExprUnset;
    children=[];
    loc=loc;
  }

let parse_foreach loc props children =
  Foreach {
    Foreach.array=get_prop_expr props "array";
    item=get_prop_expr props "item";
    index=get_prop_expr props "index";
    children=children;
    loc=loc;
  }

let parse_log loc props children =
  Log {
    Log.expr=get_prop_expr props "expr";
    label=get_prop props "label";
    loc=loc;
  }

let parse_data_model loc children =
  DataModel {
    DataModel.children=children;
    loc=loc;
  }

let parse_data loc props children =
  Data {
    Data.id=get_prop props "id";
    src=get_prop props "src";
    expr=get_prop_expr props "expr";
    (* TODO *)
    children=Some "";
    loc=loc;
  }

let parse_assign loc props children =
  Assign {
    Assign.location=get_prop_expr props "location";
    expr=get_prop_expr props "expr";
    (* TODO *)
    children=Some "";
    loc=loc;
  }

let parse_done_data loc children =
  DoneData {
    DoneData.children=children;
    loc=loc;
  }

let parse_content loc props children =
  Content {
    Content.expr=get_prop_expr props "expr";
    (* TODO *)
    children=Some "";
    loc=loc;
  }

let parse_param loc props =
  Param {
    Param.name=get_prop props "name";
    expr=get_prop_expr props "expr";
    location=get_prop_expr props "location";
    loc=loc;
  }

let parse_script loc props children =
  Script {
    Script.src=get_prop props "src";
    (* TODO *)
    children=Some "";
    loc=loc;
  }

let parse_send parse_string_list loc props children =
  Send {
    Send.event=get_prop_expr_alias props "event" "expr";
    target=get_prop_expr_alias props "target" "expr";
    t=get_prop_expr_alias props "type" "expr";
    id=get_prop_expr_alias props "id" "location";
    delay=get_prop_expr_alias props "delay" "expr";
    namelist=get_prop props "namelist" |> parse_string_list |> (List.map (fun s -> Expr s));
    children=children;
    loc=loc;
  }

let parse_cancel loc props =
  Cancel {
    Cancel.sendid=get_prop_expr_alias props "sendid" "expr";
    loc=loc;
  }

let parse_invoke parse_string_list loc props children =
  Invoke {
    Invoke.t=get_prop_expr_alias props "type" "expr";
    src=get_prop_expr_alias props "src" "expr";
    id=get_prop_expr_alias props "id" "location";
    namelist=get_prop props "namelist" |> parse_string_list |> (List.map (fun s -> Expr s));
    autoforward=get_prop_bool props "autoforward";
    children=children;
    loc=loc;
  }

let parse_finalize loc children =
  Finalize {
    Finalize.children=children;
    loc=loc;
  }

let parse_element split loc name proplist children =
  let props = Props.of_list proplist in
  let ns, el = name in
  match ns with
  | "http://www.w3.org/2005/07/scxml" -> (
    match el with
    | "scxml" -> SCEl (parse_scxml (parse_string_list split) loc props (filter_children children))
    | "state" -> SCEl (parse_state (parse_string_list split) loc props (filter_children children))
    | "parallel" -> SCEl (parse_parallel loc props (filter_children children))
    | "transition" -> SCEl (parse_transition (parse_string_list split) loc props (filter_children children))
    | "initial" -> SCEl (parse_initial  loc props (filter_children children))
    | "final" -> SCEl (parse_final loc props (filter_children children))
    | "onentry" -> SCEl (parse_on_entry loc props (filter_children children))
    | "onexit" -> SCEl (parse_on_exit loc props (filter_children children))
    | "history" -> SCEl (parse_history loc props (filter_children children))
    | "raise" -> SCEl (parse_raise loc props)
    | "if" -> SCEl (parse_if loc props (filter_children children))
    | "ifelse" -> SCEl (parse_if_else loc props)
    | "else" -> SCEl (parse_else loc)
    | "foreach" -> SCEl (parse_foreach loc props (filter_children children))
    | "log" -> SCEl (parse_log loc props (filter_children children))
    | "datamodel" -> SCEl (parse_data_model loc (filter_children children))
    | "data" -> SCEl (parse_data loc props (filter_children children))
    | "assign" -> SCEl (parse_assign loc props (filter_children children))
    | "donedata" -> SCEl (parse_done_data loc (filter_children children))
    | "content" -> SCEl (parse_content loc props children)
    | "param" -> SCEl (parse_param loc props)
    | "script" -> SCEl (parse_script loc props children)
    | "send" -> SCEl (parse_send (parse_string_list split) loc props (filter_children children))
    | "cancel" -> SCEl (parse_cancel loc props)
    | "invoke" -> SCEl (parse_invoke (parse_string_list split) loc props (filter_children children))
    | "finalize" -> SCEl (parse_finalize loc (filter_children children))
    | _ -> None
  )
  | _ -> None

let unwrap root =
  match root with
  | Some (SCEl (Document doc)) -> Some doc
  | _ -> None
