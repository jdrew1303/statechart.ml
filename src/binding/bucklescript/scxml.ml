type parser
external create_parser : 'a -> parser = "parse5/lib/sax" [@@bs.new] [@@bs.module]

external parse : parser -> string -> unit = "parse" [@@bs.send "end"]
external on : parser -> string -> 'a -> unit = "on" [@@bs.send "on"]

external push : 'a array -> 'a -> unit = "push" [@@bs.send "push"]
external pop : 'a array -> 'a = "pop" [@@bs.send "pop"]

type attrs
type loc = int * int

type tag = {
  name : string;
  ns : string;
  attrs : attrs;
  children : tag array;
  open_loc : loc;
  mutable close_loc : loc;
  mutable parsed : Statechart_scxml_parser.html list;
}

let get_xmlns : attrs -> string Js.Undefined.t = [%bs.raw{|
  function(attrs) {
    for (var i = 0, attr; i < attrs.length; i++) {
      attr = attrs[i];
      if (attr.name === 'xmlns') return attr.value;
    }
    return undefined;
  }
|}]

external get_line : 'a -> int = "line" [@@bs.get]
external get_col : 'a -> int = "col" [@@bs.get]

let get_parent stack =
  stack.((Array.length stack) - 1)

let get_xmlns_default attrs stack =
  match Js.Undefined.to_opt (get_xmlns attrs) with
  | Some ns -> ns
  | None ->
    if (Array.length stack) == 0
    then "http://www.w3.org/2005/07/scxml"
    else get_parent stack

let get_siblings stack dom =
  if (Array.length stack) == 0
  then dom
  else (get_parent stack).children

let ws = [%bs.re "/(?:\\s+)/g"]
external split : string -> 'a -> string array = "split" [@@bs.send "split"]

let parse_string_list str =
  List.filter (fun m ->
    match m with
    | "" -> false
    | _ -> true
  ) (Array.to_list (split str ws))

let format_attr ns key value =
  (ns, key), value

let format_attrs : attrs -> Statechart_scxml_parser.prop array = [%bs.raw{|
  function(attrs) {
    return attrs.map(function(attr) {
      return format_attr("", attr.name, attr.value);
    });
  }
|}]

let parse_children children =
  List.map (fun child ->
    let name = child.name in
    let ns = child.ns in
    let attrs = format_attrs child.attrs in
    let attrs = Array.to_list attrs in
    let children = child.parsed in
    let loc = Some (child.open_loc, child.close_loc) in
    Statechart_scxml_parser.parse_element parse_string_list loc (ns, name) attrs children
  ) (Array.to_list children)

let pick_loc op last =
  match op, last with
  | (a, a1), (b, b1) when a == b && a1 > b1 -> op
  | (a, _), (b, _) when a > b -> op
  | _ -> last

let of_string str =
  let dom = [||] in
  let tag_stack = [||] in
  let ns_stack = [||] in
  let last_loc = ref (0, 0) in

  let parser = create_parser [%bs.obj {
    locationInfo = Js.true_;
  }] in

  let addDomElement elem =
    let siblings = get_siblings tag_stack dom in
    push siblings elem
  in

  let rec endTag name location =
    match Js.Undefined.to_opt (pop tag_stack) with
    | None -> ()
    | Some elem -> (
      elem.close_loc <- (match Js.Undefined.to_opt location with
      | Some l -> (get_line l), (get_col l)
      | None -> (pick_loc elem.open_loc !last_loc));

      last_loc := elem.close_loc;

      let _ = pop ns_stack in

      elem.parsed <- parse_children elem.children;

      if name != elem.name
      then endTag name location
    )
  in

  on parser "endTag" endTag;

  on parser "startTag" (fun name attrs selfClosing location ->
    let ns = get_xmlns_default attrs ns_stack in
    let open_loc = (get_line location), (get_col location) in
    let elem = {
      name; ns; attrs;
      children=[||];
      parsed=[];
      open_loc;
      close_loc=open_loc;
    } in
    addDomElement elem;
    push tag_stack elem;
    push ns_stack ns;
    if selfClosing then endTag name location
  );

  on parser "text" (fun text ->
    (* TODO *)
    ()
  );

  parse parser str;

  endTag "" Js.undefined;

  match parse_children dom with
  | hd :: _ -> Statechart_scxml_parser.unwrap (Some hd)
  | _ -> Statechart_scxml_parser.unwrap None
