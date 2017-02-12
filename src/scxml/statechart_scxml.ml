open Statechart_scxml_parser

let ws_re = Re.compile (Re_emacs.re "[ \t\n\r]+")
let ws_split str =
  let tokens = Re.split ws_re str in
  List.filter (fun s -> match s with
    | "" -> false
    | _ -> true
  ) tokens

let of_signals signals =
  signals
  |> Markup.tree
    ~text:(fun ss -> Text (String.concat "" ss))
    ~element:(parse_element ws_split None)
  |> unwrap

let of_stream stream =
  let parser = Markup.parse_xml stream in
  parser
  |> Markup.signals
  |> Markup.tree
    ~text:(fun ss -> Text (String.concat "" ss))
    ~element:(fun n p c ->
      let loc = Markup.location parser in
      parse_element ws_split (Some (loc, loc)) n p c
    )
  |> unwrap

let of_string str =
  of_stream (Markup.string str)

let of_buffer buffer =
  of_stream (Markup.buffer buffer)

let of_channel chn =
  of_stream (Markup.channel chn)
