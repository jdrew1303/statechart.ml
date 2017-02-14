type t
type datamodel
type document
type event
type executable
val load : Statechart_executable.document -> document
val start : document -> datamodel -> t
val handle_event : t -> document -> event -> t
val synchronize : t -> document -> t
val invoke : t -> document -> t
val stop : t -> document -> t

val get_configuration : t -> int array
val get_configuration_names : t -> document -> string array
val get_datamodel : t -> datamodel
val put_datamodel : t -> datamodel -> t
val get_executions : t -> executable array
