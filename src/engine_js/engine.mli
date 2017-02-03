type datamodel
type engine
type document
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
val get_configuration : engine -> configuration
val get_configuration_names : engine -> document -> string array
val get_datamodel : engine -> datamodel
val put_datamodel : engine -> datamodel -> engine
val get_invocations : engine -> invocations
val get_queues : engine -> (internal * executions)
val is_running : engine -> bool
