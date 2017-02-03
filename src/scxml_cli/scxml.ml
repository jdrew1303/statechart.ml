open Cmdliner
open Statechart_scxml
open Statechart_analyzer
open Statechart_analyzer_types
open Statechart_analyzer_printer

let handle_result res =
  match res with
  | Some doc ->
    let doc, _ = assign_state_ids doc in
    dump_document doc
  | None -> print_endline "Invalid document"

let compile src =
  src
  |> open_in_bin
  |> from_channel
  |> handle_result;
  `Ok true

let src =
  let doc = "SCXML file to compile." in
  Arg.(required & pos ~rev:true 0 (some string) None & info [] ~docv:"SOURCE" ~doc)

let cmd =
  let doc = "compile scxml" in
  let man = [
    `S "Testing";
    `P "Hello";
    `S "Another test";
    `P "Hello again";
  ] in
  Term.(ret (const compile $ src)),
  Term.info "statechart scxml" ~version:"0.1.0" ~doc ~man

let () = match Term.eval cmd with `Error _ -> exit 1 | _ -> exit 0
