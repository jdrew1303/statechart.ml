open Statechart

(* TODO expose this as a "public" type *)
type configuration

module type ContextType =
  sig
    type t
    (** The type of the context *)

    val update_configuration : t -> configuration -> t
    (** Update the context configuration *)

    val get_configuration : t -> configuration
    (** Get the context configuration *)

    val query : t -> expression -> bool
    (** Query the context with the expression.
        This should be a pure function with no side-effects *)

    val execute : t -> expression -> t
    (** Execute expression inside the context.
        This doesn't have the actually execute at this point:
        it can be applied at a later point once it's outside of the
        interpretation. *)

    val invoke : t -> invoke -> t
    (** Invoke another service inside the context.
        This doesn't have the actually execute at this point:
        it can be applied at a later point once it's outside of the
        interpretation. *)

    val stop : t -> t
    (** Perform any needed cleanup here *)
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
