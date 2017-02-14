type datamodels

let prepare_datamodels : datamodels -> (string * Statechart_datamodel.parser) array = [%bs.raw{|
  function(datamodels) {
    if (!datamodels) return {};
    return Object.keys(datamodels).map(function(k) {
      return [k, datamodels[k]];
    });
  }
|}]

type line_column = <
  line : int;
  column : int;
> Js.t

type message = <
  location : <
    start : line_column;
    finish : line_column;
  > Js.t;
  info : string;
> Js.t

let format_lc lc =
  let l, c = lc in
  [%bs.obj {
    line = l;
    column = c;
  }]

let format_loc loc =
  let start, finish = loc in
  [%bs.obj {
    start = format_lc start;
    finish = format_lc finish;
  }]

let format_errors l =
  let l = List.map (fun err ->
    let loc, info = err in
    [%bs.obj {
      location = format_loc loc;
      info = info;
    }]
  ) l in
  match l with
  | [] -> Js.undefined
  | _ -> Js.Undefined.return (Array.of_list l)

let parse doc datamodels =
  let doc, errs = Statechart.parse doc (prepare_datamodels datamodels) in
  [%bs.obj {
    document = doc;
    errors = format_errors errs;
  }]

let translate = Statechart.translate

let compile doc datamodels =
  match Statechart.parse doc (prepare_datamodels datamodels) with
  | doc, [] -> ( match Statechart.validate doc with
    | [], warnings -> [%bs.obj {
        document = Js.Undefined.return (translate doc);
        warnings = format_errors warnings;
        errors = Js.undefined;
      }]
    | errors, warnings -> [%bs.obj {
      document = Js.undefined;
      warnings = format_errors warnings;
      errors = format_errors errors;
    }]
  )
  | _, errors -> [%bs.obj {
    document = Js.undefined;
    warnings = Js.undefined;
    errors = format_errors errors;
  }]

let validate doc =
  let errors, warnings = Statechart.validate doc in
  [%bs.obj {
    warnings = format_errors warnings;
    errors = format_errors errors;
  }]
