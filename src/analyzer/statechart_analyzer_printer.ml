open Statechart_analyzer_types

let print_type t indent =
  print_endline (indent ^ "==== " ^ t ^ " ====")

let print_type_close indent =
  print_endline (indent ^ "====");
  print_newline ()

let print_kv key value indent =
  print_endline (indent ^ "  " ^ key ^ " = " ^ value)

let print_kv_opt key value indent =
  match value with
  | Some value -> print_kv key value indent
  | None -> print_string ""

let print_kv_list key value indent =
  let v = String.concat ", " value in
  print_kv key ("[" ^ v ^ "]") indent

let print_binding doc indent =
  match doc.Document.binding with
  | `early -> print_kv "binding" "early" indent
  | `late -> print_kv "binding" "late" indent

let rec print_document indent el =
  let indent = "" in
  print_type "Document" indent;
  print_kv_opt "name" el.Document.name indent;
  print_kv_list "initial" el.Document.initial indent;
  print_kv_opt "datamodel" el.Document.data_model indent;
  print_binding el indent;
  print_children indent el.Document.children

and print_state indent el =
  print_type "State" indent;
  print_kv_opt "id" el.State.id indent;
  print_kv_list "initial" el.State.initial indent;
  print_children indent el.State.children

and print_parallel indent el =
  print_type "Parallel" indent;
  print_kv_opt "id" el.Parallel.id indent;
  print_children indent el.Parallel.children

and print_transition indent el =
  print_type "Transition" indent;
  print_kv_list "event" el.Transition.event indent;
  print_kv_opt "cond" el.Transition.cond indent;
  print_kv_list "target" el.Transition.target indent;
  print_children indent el.Transition.children

and print_initial indent el =
  print_type "Initial" indent;
  print_children indent el.Initial.children

and print_final indent el =
  print_type "Final" indent;
  print_kv_opt "id" el.Final.id indent;
  print_children indent el.Final.children

and print_on_entry indent el =
  print_type "On Entry" indent;
  print_children indent el.OnEntry.children

and print_on_exit indent el =
  print_type "On Exit" indent;
  print_children indent el.OnExit.children

and print_history indent el =
  print_type "History" indent;
  print_kv_opt "id" el.History.id indent;
  (* TODO print history type *)
  print_children indent el.History.children

and print_raise indent el =
  print_type "Raise" indent;
  print_kv_opt "event" el.Raise.event indent;
  print_type_close indent

and print_foreach indent el =
  print_type "Foreach" indent;
  print_kv_opt "array" el.Foreach.array indent;
  print_kv_opt "item" el.Foreach.item indent;
  print_kv_opt "index" el.Foreach.index indent;
  print_children indent el.Foreach.children

and print_log indent el =
  print_type "Log" indent;
  print_kv_opt "label" el.Log.label indent;
  print_kv_opt "expr" el.Log.expr indent;
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
  (* | Case e -> print_case indent e *)
  | Foreach e -> print_foreach indent e
  | Log e -> print_log indent e


  | _ -> print_endline "UNHANDLED"
and print_children indent children =
  match children with
  | [] -> print_type_close indent
  | _ ->
    print_newline ();
    List.iter (print_el indent) children;
    print_type_close indent

let dump_document doc =
  print_document "" doc
