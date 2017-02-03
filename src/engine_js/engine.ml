open Statechart_interpreter

module Engine = Make(struct
  (* This is just a foreign empty js object *)
  type datamodel
  type executable = datamodel -> datamodel
  type event_pattern = string

  let match_event pattern event =
    (* TODO match a regex *)
    true
end)

include Engine
