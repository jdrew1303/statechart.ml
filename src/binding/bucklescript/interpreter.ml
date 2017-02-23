open Statechart_interpreter

module Interpreter = Make(struct
  (* This is just a foreign empty js object *)
  type datamodel
  type executable = datamodel -> datamodel

  let load_executable expr =
    (* Js.log expr; *)
    fun datamodel ->
      datamodel

  let load_query expr =
    (* Js.log expr; *)
    fun datamodel ->
      true

  let load_event_match event_list =
    (* Js.log event_list; *)
    fun event -> true
end)

include Interpreter
