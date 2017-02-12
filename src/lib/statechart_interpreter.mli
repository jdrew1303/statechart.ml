module type Engine = sig
  type datamodel
  type executable

  val load_executable : Statechart_executable.expression -> executable
  val load_query : Statechart_executable.expression -> (datamodel -> bool)
  val load_event_match : string array -> (string -> bool)
end

module type Interpreter = sig
  type t
  type datamodel
  type document
  type invoke
  type executable
  type event

  val load : Statechart_executable.document -> document

  val start : document -> datamodel -> t
  val handle_event : document -> t -> event -> t
  val synchronize : document -> t -> t
  val invoke : document -> t -> t
  val stop : document -> t -> t

  val get_configuration : t -> int array
  val get_configuration_names : t -> document -> string array
  val get_datamodel : t -> datamodel
  val put_datamodel : t -> datamodel -> t
  val get_executions : t -> executable array
end

module Make (Eng : Engine) :
  Interpreter with type datamodel = Eng.datamodel
