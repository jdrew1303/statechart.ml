open Markup
open Statechart_analyzer_types

val from_stream : (char, Markup.sync) Markup.stream -> document option
val from_string : string -> document option
val from_channel : in_channel -> document option
