open Cmdliner
open Statechart_scxml
open Statechart_t

let datamodels = [|
    "ecmascript", Statechart_ecmascript.parse;
  |]

let handle_result res =
  match res with
  | Some doc ->
    let document, dm_errors = Statechart.parse doc datamodels in
    let _errors, _warnings = Statechart_validator.validate document in
    let document = Statechart.translate document in
    let iolist = Statechart_format.gen_document document in
    Statechart_format_runtime.to_channel stdout iolist;
  | None -> prerr_endline "Invalid document"

let compile src =
  src
  |> open_in_bin
  |> of_channel
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
