open Markup
open Statechart
open Statechart_format

module Prop = struct
  type t = Markup.name * string
  let compare a b =
    let (a_n, _) = a in
    let (b_n, _) = b in
    Pervasives.compare a_n b_n
end
module Props = Set.Make(Prop)

let get_ns_prop props ns name =
  try
    let (_, value) = (Props.find ((ns, name), "") props) in
    Some value
  with
  | Not_found -> None

let get_prop props name =
  get_ns_prop props "" name

type html = Text of string | Element of Markup.name * Props.t * html list

let parse_scxml props children =
  {
    Document.name=get_prop props "name";
    Document.initial_transitions=[];
    Document.states=[||];
  }

let parse_root root =
  match root with
  | Some (Element (("http://www.w3.org/2005/07/scxml", "scxml"), props, children)) ->
    Some (parse_scxml props children)
  | _ ->
    (* TODO return error here *)
    None

let from_stream stream =
  stream
  |> parse_xml
  |> Markup.signals
  |> tree
    ~text:(fun ss -> Text (String.concat "" ss))
    ~element:(fun name proplist children -> Element (name, (Props.of_list proplist), children))
  |> parse_root

let from_string str =
  from_stream (string str)

let from_channel chn =
  from_stream (channel chn)
