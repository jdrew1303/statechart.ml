open Statechart

type result = Program of Statechart.expression
            | Error of (int * string) list

type parser = string -> result

module DatamodelMap = Map.Make(struct
  type t = string
  let compare = compare
end)
type datamodel_map = parser DatamodelMap.t

let of_list l =
  List.fold_left (fun acc kv ->
    let k, v = kv in
    DatamodelMap.add k v acc
  ) DatamodelMap.empty l
