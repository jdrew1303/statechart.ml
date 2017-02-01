open Statechart_interpreter_engine

module type Interpreter = sig
  type t
  type configuration
  type document
  type executable
  val start : t -> document -> t
  val stop : t -> document -> t
end

module Make (Eng : Engine) :
  Interpreter with type t = Eng.t
              with type configuration = Eng.configuration
              with type document = Eng.document
              with type executable = Eng.executable
