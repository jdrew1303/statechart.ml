open Statechart
open Crunch

let get_description path =
  let parts = String.split_on_char '.' path in
  let path = parts |> List.rev |> List.tl |> List.rev |> String.concat "." in
  let chn = open_in (path ^ ".description") in
  let length = in_channel_length chn in
  let desc = Bytes.create length in
  really_input chn desc 0 length;
  close_in chn;
  desc

let w3_test path description () =
  prerr_endline "PATH";
  prerr_endline ("  " ^ path);
  prerr_endline "DESCRIPTION:";
  prerr_endline ("  " ^ description);
  let channel = open_in path in
  match Statechart_scxml.from_channel channel with
  | None -> ()
  | Some document -> (
    let document = Statechart_analyzer.analyze document in
    (* let document = Statechart_translator.translate document in *)
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
  Alcotest.run "W3C" [
    "W3C SCXML (Mandatory)", w3c_tests "mandatory" true;
    "W3C SCXML (Optional)", w3c_tests "optional" false;
  ]
