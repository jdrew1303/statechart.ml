open Statechart_interpreter

module Engine = Make(struct
  (* This is just a foreign empty js object *)
  type datamodel
  type executable = datamodel -> datamodel

  let load_executable expr =
    fun datamodel -> datamodel

  let load_query expr =
    fun datamodel -> true

  let load_event_match event_list =
    fun event -> true
end)

include Engine
