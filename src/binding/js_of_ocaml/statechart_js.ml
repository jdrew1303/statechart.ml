let default_fun = fun str -> Statechart_datamodel.Error []

let js_table_to_tuple_array tbl =
  let keys = Jstable.keys tbl in
  let l = List.map (fun k ->
    let key = Js.to_string k in
    let fn = (Jstable.find tbl k) in
    let fn = (Js.Optdef.get fn (fun () -> default_fun)) in
    key, fn
  ) keys in
  Array.of_list l

let format_errors l =
  let l = List.map (fun err ->
    let line, info = err in
    (object%js
      val line = line
      val info = info
    end)
  ) l in
  match l with
  | [] -> Js.undefined
  | _ -> Js.def (Js.array (Array.of_list l))

let parse doc datamodels =
  Statechart.parse doc (js_table_to_tuple_array datamodels)

let validate = Statechart.validate
let translate = Statechart.translate

let _ =
  let datamodels = Js.Unsafe.obj [|
    "ecmascript", Js.Unsafe.inject Statechart_ecmascript.parse;
  |] in

  Js.export_all (object%js
  method parse = parse
  method validate = validate
  method translate = translate
  val scxml = (object%js
    method fromString str =
      str
      |> Js.to_string
      |> Statechart_scxml.from_string
  end)
  val datamodels = datamodels
  val format = (object%js
    method decode str =
      str
      |> Js.to_string
      |> Statechart_format_runtime.init_from_string
      |> Statechart_format.parse_document
    method encode doc =
      doc
      |> Statechart_format.gen_document
      |> Statechart_format_runtime.to_string
      |> Js.string
  end)

  method compile doc datamodels =
    match parse doc datamodels with
    | doc, [] -> ( match validate doc with
      | [], warnings -> (object%js
          val document = Js.def (translate doc)
          val warnings = format_errors warnings
          val errors = Js.undefined
        end)
      | errors, warnings -> (object%js
        val document = Js.undefined
        val warnings = format_errors warnings
        val errors = format_errors errors
      end)
    )
    | _, errors -> (object%js
      val document = Js.undefined
      val warnings = Js.undefined
      val errors = format_errors errors
    end)
end)
