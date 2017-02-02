open Statechart_interpreter_engine

module type Interpreter = sig
  type t
  type configuration
  type document
  type executable
  type event
  val start : t -> document -> t * configuration
  val handle_internal_event : t -> document -> configuration -> event -> t * configuration
  val handle_external_event : t -> document -> configuration -> event -> t * configuration
  val finalize_macrostep : t -> document -> configuration -> t
  val stop : t -> document -> configuration -> t
end

module Make (Eng : Engine) :
  Interpreter with type t = Eng.t
              with type document = Eng.document
              with type executable = Eng.executable
              with type event = Eng.event
