open Statechart_t

type prop_name = (string * string)
type prop = prop_name * string
type parse_string_list = string -> string list

type html = Text of string
          | SCEl of statechart_el
          | None

val parse_element : parse_string_list -> Statechart_t.loc option -> string * string -> prop list -> html list -> html
val unwrap : html option -> document option
