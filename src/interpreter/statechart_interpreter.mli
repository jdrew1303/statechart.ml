open Statechart_interpreter_engine

module type Interpreter = sig
  type engine
  type datamodel
  type document
  type executable
  type event
  val start : datamodel -> document -> engine
  val handle_internal_event : engine -> document -> event -> engine
  val synchronize : engine -> document -> engine
  val handle_external_event : engine -> document -> event -> engine
  val finalize_macrostep : engine -> document -> engine
  val stop : engine -> document -> engine

  type configuration
  type internal
  type executions
  type invocations
  type cancellations
  val get_configuration : engine -> configuration
  val put_configuration : engine -> configuration -> engine
  val get_datamodel : engine -> datamodel
  val put_datamodel : engine -> datamodel -> engine
  val get_queues : engine -> (internal * executions * invocations * cancellations)
  val is_running : engine -> bool
end

module Make (Eng : Engine) :
  Interpreter with type datamodel = Eng.datamodel
              with type executable = Eng.executable
