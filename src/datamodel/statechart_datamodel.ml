open Statechart

type result = Program of Statechart.expression list
            | Error of (int * string) list

type parser = string -> result

module DatamodelMap = Map.Make(struct
  type t = string
  let compare = compare
end)
type datamodel_map = parser DatamodelMap.t
