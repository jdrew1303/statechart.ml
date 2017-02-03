open Statechart_interpreter

(* This is just a foreign empty js object *)
type datamodel_

module Engine = Make(struct
  type datamodel = datamodel_
  type executable = datamodel -> datamodel
  type event_pattern = string

  let match_event pattern event =
    (* TODO match a regex *)
    true
end)

include Engine
