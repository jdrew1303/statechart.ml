type t
type datamodel
type document
type event
val load : Statechart.document -> document
val start : document -> datamodel -> t
val handle_event : document -> t -> event -> t
val synchronize : document -> t -> t
val invoke : document -> t -> t
val stop : document -> t -> t

(* type configuration
type internal
type executions
type invocations
val get_configuration : t -> configuration
val get_configuration_names : t -> document -> string array
val get_datamodel : t -> datamodel
val put_datamodel : t -> datamodel -> t
val get_invocations : t -> invocations
val get_queues : t -> (internal * executions)
val is_running : t -> bool *)
