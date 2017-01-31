open Statechart_analyzer_types

let print_type t indent =
  print_string (indent ^ t ^ "(")

let print_type_close indent =
  print_newline ()

let print_ws ws =
  match ws with
  | " " -> print_string ""
  | _ -> print_string ws

let print_opt v default =
  match v with
  | Some v -> print_string v
  | None -> print_string default

let print_kv key value ws =
  print_string (key ^ "=\"" ^ value ^ "\"" ^ ws)

let print_kv_nq key value ws =
  print_string (key ^ "=(" ^ value ^ ")" ^ ws)

let print_kv_opt key value ws =
  match value with
  | Some value -> print_kv key value ws
  | None -> print_ws ws

let print_kv_opt_nq key value ws =
  match value with
  | Some value -> print_kv_nq key value ws
  | None -> print_ws ws

let print_kv_expr key value ws =
  match value with
  | Expr v -> print_kv_nq key v ws
  | ExprValue v -> print_kv key v ws
  | ExprUnset -> print_ws ws

let print_kv_list key value ws =
  let v = String.concat "\", \"" value in
  match v with
  | "" -> print_ws ws
  | _ -> print_kv_nq key ("[\"" ^ v ^ "\"]") ws

let print_binding doc ws =
  match doc.Document.binding with
  | `early -> print_kv "binding" "early" ws
  | `late -> print_kv "binding" "late" ws

let rec print_document indent el =
  let indent = "" in
  print_type "scxml" indent;
  print_kv_opt "name" el.Document.name " ";
  print_kv_list "initial" el.Document.initial " ";
  print_kv_opt "datamodel" el.Document.data_model " ";
  print_binding el " ";
  print_kv "xmlns" "http://www.w3.org/2005/07/scxml" " ";
  print_kv "version" "1.0" ")";
  print_children indent el.Document.children

and print_state indent el =
  print_type "state" indent;
  print_kv_opt "id" el.State.id " ";
  print_kv_list "initial" el.State.initial ")";
  print_children indent el.State.children

and print_parallel indent el =
  print_type "parallel" indent;
  print_kv_opt "id" el.Parallel.id ")";
  print_children indent el.Parallel.children

and print_transition indent el =
  print_type "transition" indent;
  print_kv_list "event" el.Transition.event " ";
  print_kv_opt_nq "cond" el.Transition.cond " ";
  print_kv_list "target" el.Transition.target ")";
  print_children indent el.Transition.children

and print_initial indent el =
  print_type "initial" indent;
  print_string ")";
  print_children indent el.Initial.children

and print_final indent el =
  print_type "final" indent;
  print_kv_opt "id" el.Final.id ")";
  print_children indent el.Final.children

and print_on_entry indent el =
  print_type "onentry" indent;
  print_string ")";
  print_children indent el.OnEntry.children

and print_on_exit indent el =
  print_type "onext" indent;
  print_string ")";
  print_children indent el.OnExit.children

and print_history indent el =
  print_type "history" indent;
  print_kv_opt "id" el.History.id ")";
  (* TODO print history type *)
  print_children indent el.History.children

and print_raise indent el =
  print_type "raise" indent;
  print_kv_opt "event" el.Raise.event ")";
  print_type_close indent

and print_foreach indent el =
  print_string (indent ^ "each ");
  print_opt el.Foreach.item "$$item";
  print_string ", ";
  print_opt el.Foreach.item "$$index";
  print_string " in ";
  print_opt el.Foreach.array "[]";
  print_children indent el.Foreach.children

and print_log indent el =
  print_type "log" indent;
  print_kv_opt "label" el.Log.label " ";
  print_kv_opt_nq "expr" el.Log.expr ")";
  print_type_close indent

and print_data_model indent el =
  print_type "datamodel" indent;
  print_string ")";
  print_children indent el.DataModel.children

and print_data indent el =
  print_string (indent ^ "- " );
  print_opt el.Data.id "$$id";
  print_string " = ";
  match el with
  | {Data.expr=Some e} -> print_string e
  | {Data.src=Some e} -> print_string ("fetch(\"" ^ e ^ "\")")
  | _ -> print_string "null";
  print_string ";";
  (* TODO print children *)
  print_type_close indent

and print_assign indent el =
  print_string (indent ^ "- " );
  print_opt el.Assign.location "$$location";
  print_string " = ";
  print_opt el.Assign.expr "null";
  print_string ";";
  (* TODO print children *)
  print_type_close indent

and print_done_data indent el =
  print_type "donedata" indent;
  print_string ")";
  print_children indent el.DoneData.children

and print_content indent el =
  print_type "content" indent;
  print_kv_opt_nq "expr" el.Content.expr ")";
  (* TODO print children *)
  print_type_close indent

and print_param indent el =
  print_type "param" indent;
  print_kv_opt "name" el.Param.name " ";
  print_kv_opt_nq "expr" el.Param.expr " ";
  print_kv_opt_nq "location" el.Param.location ")";
  print_type_close indent

and print_script indent el =
  print_type "script" indent;
  print_kv_opt "src" el.Script.src ")";
  print_type_close indent

and print_send indent el =
  print_type "send" indent;
  print_kv_expr "event" el.Send.event " ";
  print_kv_expr "target" el.Send.target " ";
  print_kv_expr "type" el.Send.t " ";
  print_kv_expr "id" el.Send.id " ";
  print_kv_expr "delay" el.Send.delay " ";
  print_kv_list "namelist" el.Send.namelist ")";
  print_type_close indent

and print_cancel indent el =
  print_type "cancel" indent;
  print_type_close indent

and print_invoke indent el =
  print_type "invoke" indent;
  print_type_close indent

and print_finalize indent el =
  print_type "finalize" indent;
  print_type_close indent

and print_el indent el =
  let indent = "  " ^ indent in
  match el with
  | Document e -> print_document indent e
  | State e -> print_state indent e
  | Parallel e -> print_parallel indent e
  | Transition e -> print_transition indent e
  | Initial e -> print_initial indent e
  | Final e -> print_final indent e
  | OnEntry e -> print_on_entry indent e
  | OnExit e -> print_on_exit indent e
  | History e -> print_history indent e

  | Raise e -> print_raise indent e
  | Case _ -> prerr_endline "UNHANDLED"
  | CaseClause _ -> prerr_endline "UNHANDLED"
  | CaseDefault _ -> prerr_endline "UNHANDLED"
  | Foreach e -> print_foreach indent e
  | Log e -> print_log indent e

  | DataModel e -> print_data_model indent e
  | Data e -> print_data indent e
  | Assign e -> print_assign indent e
  | DoneData e -> print_done_data indent e
  | Content e -> print_content indent e
  | Param e -> print_param indent e
  | Script e -> print_script indent e

  | Send e -> print_send indent e
  | Cancel e -> print_cancel indent e
  | Invoke e -> print_invoke indent e
  | Finalize e -> print_finalize indent e

  | Other -> prerr_endline "INVALID"
and print_children indent children =
  match children with
  | [] -> print_type_close indent
  | _ ->
    print_newline ();
    List.iter (print_el indent) children;
    print_type_close indent

let dump_document doc =
  print_document "" doc
