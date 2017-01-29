open Statechart

module type ContextType =
  sig
    type t
    val query : t -> expression -> bool
    val execute : t -> expression -> t
    val invoke : t -> invoke -> t
    val close : t -> t
  end

module type Interpreter =
  sig
    type t
    (** The type of the context *)

    val start : t -> document -> t
    (** Start the interpreter *)

    val handle_event : t -> document -> Statechart_event.t -> t
    (** Handle an event *)

    val stop : t -> t
    (** Stop the interpreter *)
  end
(** Output signature of the functor {!Interpreter.Make}. *)

module Make (Ctx : ContextType) : Interpreter with type t = Ctx.t
