open Statechart
open Statechart_executable
open Crunch

let replace_extension path ext =
  let parts = String.split_on_char '.' path in
  let path = parts |> List.rev |> List.tl |> List.rev |> String.concat "." in
  path ^ ext

let get_description path =
  let chn = open_in (replace_extension path ".description") in
  let length = in_channel_length chn in
  let desc = Bytes.create length in
  really_input chn desc 0 length;
  close_in chn;
  desc

let datamodels = [|
    "ecmascript", Statechart_ecmascript.parse;
    "null", Statechart_null.parse;
  |]

exception Type_error
let compare_state expected actual =
  if expected.State.t != actual.State.t then raise Type_error
  else (
    Alcotest.(check int) "same parent"
      expected.State.parent
      actual.State.parent;
    Alcotest.(check (array int)) "same children"
      expected.State.children
      actual.State.children;
    Alcotest.(check (array int)) "same ancestors"
      expected.State.ancestors
      actual.State.ancestors;
    Alcotest.(check (array int)) "same completion"
      expected.State.completion
      actual.State.completion;
    Alcotest.(check bool) "same has_history"
      expected.State.has_history
      actual.State.has_history;
  );
  ()

let compare_transition expected actual =
  if expected.Transition.t != actual.Transition.t then raise Type_error
  else (
    Alcotest.(check int) "same source"
      expected.Transition.source
      actual.Transition.source;
    Alcotest.(check (array string)) "same events"
      expected.Transition.events
      actual.Transition.events;
    Alcotest.(check (array int)) "same targets"
      expected.Transition.targets
      actual.Transition.targets;
    Alcotest.(check (array int)) "same exits"
      expected.Transition.exits
      actual.Transition.exits;
    Alcotest.(check (array int)) "same conflicts"
      expected.Transition.conflicts
      actual.Transition.conflicts;
  );
  ()

let compare_doc expected actual =
  Alcotest.(check int) "same state size"
    (Array.length expected.Document.states)
    (Array.length actual.Document.states);
  Alcotest.(check int) "same transition size"
    (Array.length expected.Document.transitions)
    (Array.length actual.Document.transitions);
  Array.iter2 compare_state expected.Document.states actual.Document.states;
  Array.iter2 compare_transition expected.Document.transitions actual.Document.transitions;
  ()

let w3_test path description () =
  prerr_endline "PATH";
  prerr_endline ("  " ^ path);
  prerr_endline "DESCRIPTION:";
  prerr_endline ("  " ^ description);
  let channel = open_in path in
  match Statechart_scxml.of_channel channel with
  | None -> ()
  | Some document -> (
    let document, dm_errors = Statechart.parse document datamodels in
    let _errors, _warnings = Statechart.validate document in
    let document = Statechart.translate document in
    let exp_chn = open_in (replace_extension path ".pb") in
    let exp_piqi = Statechart_format_runtime.init_from_channel exp_chn in
    let expected = Statechart_format.parse_document exp_piqi in
    close_in exp_chn;
    compare_doc expected document;
    ()
  )

let w3c_tests kind required =
  let tests = ref [] in
  let cwd = Sys.getcwd () in
  walk_directory_tree ["scxml"] (fun dir name ->
    let path = dir ^ "/" ^ name in
    let description = get_description path in
    (* TODO if not required wrap in a function and make it a warning *)
    let fn = w3_test path description in
    let test = name, `Quick, fn in
    tests := test :: !tests;
  ) (cwd ^ "/w3c/" ^ kind);
  Sys.chdir cwd;
  List.rev !tests

let () =
  Alcotest.run "Statechart" [
    "W3C SCXML (Mandatory)", w3c_tests "mandatory" true;
    "W3C SCXML (Optional)", w3c_tests "optional" false;
  ]
