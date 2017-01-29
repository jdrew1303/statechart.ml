open Statechart_format_piqi

(* let printf = Printf.printf *)

(* let print_doc doc = *)
  (* printf "  Name: %s\n" doc.Document.name *)

let parse_string str =
  let buf = Statechart_format_runtime.init_from_string str in
  let document = parse_document buf in

  document
  (* print_doc document *)
