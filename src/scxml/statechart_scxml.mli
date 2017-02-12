open Markup
open Statechart_t

val of_signals : (Markup.signal, Markup.sync) Markup.stream -> document option
val of_stream : (char, Markup.sync) Markup.stream -> document option
val of_string : string -> document option
val of_buffer : Buffer.t -> document option
val of_channel : in_channel -> document option
