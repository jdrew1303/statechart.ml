module type Engine = sig
  type datamodel
  type executable

  val load_executable : Statechart.expression -> executable
  val load_query : Statechart.expression -> (datamodel -> bool)
  val load_event_match : string list -> (string -> bool)
end
