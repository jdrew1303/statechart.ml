module type Engine = sig
  type datamodel
  type executable
  type event_pattern
  val match_event : event_pattern -> string -> bool
end
