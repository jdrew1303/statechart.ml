open Statechart_interpreter_engine

module type Interpreter = sig
  type t
  type datamodel
  type document
  type invoke
  type executable
  type event

  val load : Statechart.document -> document

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
