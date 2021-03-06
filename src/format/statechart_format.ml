open Statechart_executable
module Piqirun = Statechart_format_runtime
module Bitset = Statechart_bitset

let rec parse_float64 x = Piqirun.float_of_fixed64 x
and packed_parse_float64 x = Piqirun.float_of_packed_fixed64 x

and parse_string x = Piqirun.string_of_block x

and parse_bool x = Piqirun.bool_of_varint x
and packed_parse_bool x = Piqirun.bool_of_packed_varint x

and parse_int x = Piqirun.int_of_zigzag_varint x
and packed_parse_int x = Piqirun.int_of_packed_zigzag_varint x

and parse_float x = parse_float64 x
and packed_parse_float x = packed_parse_float64 x

and parse_binary x = Piqirun.string_of_block x

and parse_uint x = Piqirun.int_of_varint x
and packed_parse_uint x = Piqirun.int_of_packed_varint x

and parse_content x =
  let code, x = Piqirun.parse_variant x in
  match code with
    | 1 ->
        let res = parse_string x in
        `string res
    | 2 ->
        let res = parse_expression x in
        `expression res
    | 3 ->
        let res = parse_document x in
        `document res
    | _ -> Piqirun.error_variant x code

and parse_document x =
  let x = Piqirun.parse_record x in
  let _name, x = Piqirun.parse_optional_field 1 parse_string x in
  let _states, x = Piqirun.parse_repeated_field 2 parse_state x in
  let _transitions, x = Piqirun.parse_repeated_field 3 parse_transition x in
  Piqirun.check_unparsed_fields x;
  {
    Document.name = _name;
    Document.states = Array.of_list _states;
    Document.transitions = Array.of_list _transitions;
  }

and parse_expression_type x =
  match Piqirun.int32_of_signed_varint x with
    | 1l -> `block
    | 2l -> `null
    | 3l -> `list
    | 4l -> `map
    | 5l -> `bool
    | 6l -> `int
    | 7l -> `float
    | 8l -> `string
    | 9l -> `raise
    | 10l -> `case
    | 11l -> `clause
    | 12l -> `foreach
    | 13l -> `log
    | 14l -> `assign
    | 15l -> `send
    | 16l -> `cancel
    | 17l -> `is_active
    | 18l -> `equal
    | 19l -> `not_equal
    | x -> Piqirun.error_enum_const x
and packed_parse_expression_type x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 1l -> `block
    | 2l -> `null
    | 3l -> `list
    | 4l -> `map
    | 5l -> `bool
    | 6l -> `int
    | 7l -> `float
    | 8l -> `string
    | 9l -> `raise
    | 10l -> `case
    | 11l -> `clause
    | 12l -> `foreach
    | 13l -> `log
    | 14l -> `assign
    | 15l -> `send
    | 16l -> `cancel
    | 17l -> `is_active
    | 18l -> `equal
    | 19l -> `not_equal
    | x -> Piqirun.error_enum_const x

and parse_expression x =
  let x = Piqirun.parse_record x in
  let _t, x = Piqirun.parse_required_field 1 parse_expression_type x in
  let _bool_val, x = Piqirun.parse_optional_field 2 parse_bool x in
  let _int_val, x = Piqirun.parse_optional_field 3 parse_int x in
  let _float_val, x = Piqirun.parse_optional_field 4 parse_float x in
  let _string_val, x = Piqirun.parse_optional_field 5 parse_string x in
  let _args, x = Piqirun.parse_repeated_field 6 parse_expression x in
  Piqirun.check_unparsed_fields x;
  {
    Expression.t = _t;
    Expression.bool_val = _bool_val;
    Expression.int_val = _int_val;
    Expression.float_val = _float_val;
    Expression.string_val = _string_val;
    Expression.args = _args;
  }

and parse_invoke x =
  let x = Piqirun.parse_record x in
  let _t, x = Piqirun.parse_optional_field 1 parse_expression x in
  let _src, x = Piqirun.parse_optional_field 2 parse_expression x in
  let _id, x = Piqirun.parse_optional_field 3 parse_expression x in
  let _autoforward, x = Piqirun.parse_required_field 4 parse_bool x ~default:"\b\000" in
  let _params, x = Piqirun.parse_repeated_field 5 parse_param x in
  let _content, x = Piqirun.parse_optional_field 6 parse_content x in
  let _on_exit, x = Piqirun.parse_repeated_field 7 parse_expression x in
  Piqirun.check_unparsed_fields x;
  {
    Invoke.t = _t;
    Invoke.src = _src;
    Invoke.id = _id;
    Invoke.autoforward = _autoforward;
    Invoke.params = Array.of_list _params;
    Invoke.content = _content;
    Invoke.on_exit = Array.of_list _on_exit;
  }

and parse_param x =
  let x = Piqirun.parse_record x in
  let _id, x = Piqirun.parse_required_field 1 parse_string x in
  let _expression, x = Piqirun.parse_optional_field 2 parse_expression x in
  Piqirun.check_unparsed_fields x;
  {
    Param.id = _id;
    Param.expression = _expression;
  }

and parse_bitset x = parse_binary x

and parse_state_type x =
  match Piqirun.int32_of_signed_varint x with
    | 1l -> `compound
    | 2l -> `atomic
    | 3l -> `parallel
    | 4l -> `history_shallow
    | 5l -> `history_deep
    | 6l -> `initial
    | 7l -> `final
    | x -> Piqirun.error_enum_const x
and packed_parse_state_type x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 1l -> `compound
    | 2l -> `atomic
    | 3l -> `parallel
    | 4l -> `history_shallow
    | 5l -> `history_deep
    | 6l -> `initial
    | 7l -> `final
    | x -> Piqirun.error_enum_const x

and parse_state x =
  let x = Piqirun.parse_record x in
  let _t, x = Piqirun.parse_required_field 1 parse_state_type x ~default:"\b\001" in
  let _idx, x = Piqirun.parse_required_field 2 parse_uint x in
  let _id, x = Piqirun.parse_optional_field 3 parse_string x in
  let _on_enter, x = Piqirun.parse_repeated_field 4 parse_expression x in
  let _on_exit, x = Piqirun.parse_repeated_field 5 parse_expression x in
  let _invocations, x = Piqirun.parse_repeated_field 6 parse_invoke x in
  let _data, x = Piqirun.parse_repeated_field 7 parse_expression x in
  let _donedata, x = Piqirun.parse_optional_field 8 parse_expression x in
  let _parent, x = Piqirun.parse_required_field 9 parse_uint x in
  let _children, x = Piqirun.parse_required_field 10 parse_bitset x in
  let _ancestors, x = Piqirun.parse_required_field 11 parse_bitset x in
  let _completion, x = Piqirun.parse_required_field 12 parse_bitset x in
  let _transitions, x = Piqirun.parse_required_field 13 parse_bitset x in
  let _has_history, x = Piqirun.parse_required_field 14 parse_bool x ~default:"\b\000" in
  Piqirun.check_unparsed_fields x;
  {
    State.t = _t;
    State.idx = _idx;
    State.id = _id;
    State.on_enter = Array.of_list _on_enter;
    State.on_exit = Array.of_list _on_exit;
    State.invocations = Array.of_list _invocations;
    State.data = Array.of_list _data;
    State.donedata = _donedata;
    State.parent = _parent;
    State.children = Bitset.of_string _children;
    State.ancestors = Bitset.of_string _ancestors;
    State.completion =Bitset.of_string  _completion;
    State.transitions = Bitset.of_string _transitions;
    State.has_history = _has_history;
  }

and parse_transition_type x =
  match Piqirun.int32_of_signed_varint x with
    | 1l -> `external_
    | 2l -> `targetless
    | 3l -> `internal
    | 4l -> `spontaneous
    | 5l -> `history
    | 6l -> `initial
    | x -> Piqirun.error_enum_const x
and packed_parse_transition_type x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 1l -> `external_
    | 2l -> `targetless
    | 3l -> `internal
    | 4l -> `spontaneous
    | 5l -> `history
    | 6l -> `initial
    | x -> Piqirun.error_enum_const x

and parse_transition x =
  let x = Piqirun.parse_record x in
  let _t, x = Piqirun.parse_required_field 1 parse_transition_type x ~default:"\b\002" in
  let _idx, x = Piqirun.parse_required_field 2 parse_uint x in
  let _source, x = Piqirun.parse_required_field 3 parse_uint x in
  let _events, x = Piqirun.parse_repeated_field 4 parse_string x in
  let _condition, x = Piqirun.parse_optional_field 5 parse_expression x in
  let _on_transition, x = Piqirun.parse_repeated_field 6 parse_expression x in
  let _targets, x = Piqirun.parse_required_field 7 parse_bitset x in
  let _conflicts, x = Piqirun.parse_required_field 8 parse_bitset x in
  let _exits, x = Piqirun.parse_required_field 9 parse_bitset x in
  Piqirun.check_unparsed_fields x;
  {
    Transition.t = _t;
    Transition.idx = _idx;
    Transition.source = _source;
    Transition.events = Array.of_list _events;
    Transition.condition = _condition;
    Transition.on_transition = Array.of_list _on_transition;
    Transition.targets = Bitset.of_string _targets;
    Transition.conflicts = Bitset.of_string _conflicts;
    Transition.exits = Bitset.of_string _exits;
  }


let rec gen__float64 code x = Piqirun.float_to_fixed64 code x
and packed_gen__float64 x = Piqirun.float_to_packed_fixed64 x

and gen__string code x = Piqirun.string_to_block code x

and gen__bool code x = Piqirun.bool_to_varint code x
and packed_gen__bool x = Piqirun.bool_to_packed_varint x

and gen__int code x = Piqirun.int_to_zigzag_varint code x
and packed_gen__int x = Piqirun.int_to_packed_zigzag_varint x

and gen__float code x = gen__float64 code x
and packed_gen__float x = packed_gen__float64 x

and gen__binary code x = Piqirun.string_to_block code x

and gen__uint code x = Piqirun.int_to_varint code x
and packed_gen__uint x = Piqirun.int_to_packed_varint x

and gen__content code (x:content) =
  Piqirun.gen_record code [(match x with
    | `string x -> gen__string 1 x
    | `expression x -> gen__expression 2 x
    | `document x -> gen__document 3 x
  )]

and gen__document code x =
  let _name = Piqirun.gen_optional_field 1 gen__string x.Document.name in
  let _states = Piqirun.gen_repeated_field 2 gen__state (Array.to_list x.Document.states) in
  let _transitions = Piqirun.gen_repeated_field 3 gen__transition (Array.to_list x.Document.transitions) in
  Piqirun.gen_record code (_name :: _states :: _transitions :: [])

and gen__expression_type code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `block -> 1l
    | `null -> 2l
    | `list -> 3l
    | `map -> 4l
    | `bool -> 5l
    | `int -> 6l
    | `float -> 7l
    | `string -> 8l
    | `raise -> 9l
    | `case -> 10l
    | `clause -> 11l
    | `foreach -> 12l
    | `log -> 13l
    | `assign -> 14l
    | `send -> 15l
    | `cancel -> 16l
    | `is_active -> 17l
    | `equal -> 18l
    | `not_equal -> 19l
  )
and packed_gen__expression_type x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `block -> 1l
    | `null -> 2l
    | `list -> 3l
    | `map -> 4l
    | `bool -> 5l
    | `int -> 6l
    | `float -> 7l
    | `string -> 8l
    | `raise -> 9l
    | `case -> 10l
    | `clause -> 11l
    | `foreach -> 12l
    | `log -> 13l
    | `assign -> 14l
    | `send -> 15l
    | `cancel -> 16l
    | `is_active -> 17l
    | `equal -> 18l
    | `not_equal -> 19l
  )

and gen__expression code x =
  let _t = Piqirun.gen_required_field 1 gen__expression_type x.Expression.t in
  let _bool_val = Piqirun.gen_optional_field 2 gen__bool x.Expression.bool_val in
  let _int_val = Piqirun.gen_optional_field 3 gen__int x.Expression.int_val in
  let _float_val = Piqirun.gen_optional_field 4 gen__float x.Expression.float_val in
  let _string_val = Piqirun.gen_optional_field 5 gen__string x.Expression.string_val in
  let _args = Piqirun.gen_repeated_field 6 gen__expression x.Expression.args in
  Piqirun.gen_record code (_t :: _bool_val :: _int_val :: _float_val :: _string_val :: _args :: [])

and gen__invoke code x =
  let _t = Piqirun.gen_optional_field 1 gen__expression x.Invoke.t in
  let _src = Piqirun.gen_optional_field 2 gen__expression x.Invoke.src in
  let _id = Piqirun.gen_optional_field 3 gen__expression x.Invoke.id in
  let _autoforward = Piqirun.gen_required_field 4 gen__bool x.Invoke.autoforward in
  let _params = Piqirun.gen_repeated_field 5 gen__param (Array.to_list x.Invoke.params) in
  let _content = Piqirun.gen_optional_field 6 gen__content x.Invoke.content in
  let _on_exit = Piqirun.gen_repeated_field 7 gen__expression (Array.to_list x.Invoke.on_exit) in
  Piqirun.gen_record code (_t :: _src :: _id :: _autoforward :: _params :: _content :: _on_exit :: [])

and gen__param code x =
  let _id = Piqirun.gen_required_field 1 gen__string x.Param.id in
  let _expression = Piqirun.gen_optional_field 2 gen__expression x.Param.expression in
  Piqirun.gen_record code (_id :: _expression :: [])

and gen__bitset code x = gen__binary code x

and gen__state_type code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `compound -> 1l
    | `atomic -> 2l
    | `parallel -> 3l
    | `history_shallow -> 4l
    | `history_deep -> 5l
    | `initial -> 6l
    | `final -> 7l
  )
and packed_gen__state_type x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `compound -> 1l
    | `atomic -> 2l
    | `parallel -> 3l
    | `history_shallow -> 4l
    | `history_deep -> 5l
    | `initial -> 6l
    | `final -> 7l
  )

and gen__state code x =
  let _t = Piqirun.gen_required_field 1 gen__state_type x.State.t in
  let _idx = Piqirun.gen_required_field 2 gen__uint x.State.idx in
  let _id = Piqirun.gen_optional_field 3 gen__string x.State.id in
  let _on_enter = Piqirun.gen_repeated_field 4 gen__expression (Array.to_list x.State.on_enter) in
  let _on_exit = Piqirun.gen_repeated_field 5 gen__expression (Array.to_list x.State.on_exit) in
  let _invocations = Piqirun.gen_repeated_field 6 gen__invoke (Array.to_list x.State.invocations) in
  let _data = Piqirun.gen_repeated_field 7 gen__expression (Array.to_list x.State.data) in
  let _donedata = Piqirun.gen_optional_field 8 gen__expression x.State.donedata in
  let _parent = Piqirun.gen_required_field 9 gen__uint x.State.parent in
  let _children = Piqirun.gen_required_field 10 gen__bitset (Bitset.to_string x.State.children) in
  let _ancestors = Piqirun.gen_required_field 11 gen__bitset (Bitset.to_string x.State.ancestors) in
  let _completion = Piqirun.gen_required_field 12 gen__bitset (Bitset.to_string x.State.completion) in
  let _transitions = Piqirun.gen_required_field 13 gen__bitset (Bitset.to_string x.State.transitions) in
  let _has_history = Piqirun.gen_required_field 14 gen__bool x.State.has_history in
  Piqirun.gen_record code (_t :: _idx :: _id :: _on_enter :: _on_exit :: _invocations :: _data :: _donedata :: _parent :: _children :: _ancestors :: _completion :: _transitions :: _has_history :: [])

and gen__transition_type code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `external_ -> 1l
    | `targetless -> 2l
    | `internal -> 3l
    | `spontaneous -> 4l
    | `history -> 5l
    | `initial -> 6l
  )
and packed_gen__transition_type x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `external_ -> 1l
    | `targetless -> 2l
    | `internal -> 3l
    | `spontaneous -> 4l
    | `history -> 5l
    | `initial -> 6l
  )

and gen__transition code x =
  let _t = Piqirun.gen_required_field 1 gen__transition_type x.Transition.t in
  let _idx = Piqirun.gen_required_field 2 gen__uint x.Transition.idx in
  let _source = Piqirun.gen_required_field 3 gen__uint x.Transition.source in
  let _events = Piqirun.gen_repeated_field 4 gen__string (Array.to_list x.Transition.events) in
  let _condition = Piqirun.gen_optional_field 5 gen__expression x.Transition.condition in
  let _on_transition = Piqirun.gen_repeated_field 6 gen__expression (Array.to_list x.Transition.on_transition) in
  let _targets = Piqirun.gen_required_field 7 gen__bitset (Bitset.to_string x.Transition.targets) in
  let _conflicts = Piqirun.gen_required_field 8 gen__bitset (Bitset.to_string x.Transition.conflicts) in
  let _exits = Piqirun.gen_required_field 9 gen__bitset (Bitset.to_string x.Transition.exits) in
  Piqirun.gen_record code (_t :: _idx :: _source :: _events :: _condition :: _on_transition :: _targets :: _conflicts :: _exits :: [])


let gen_float64 x = gen__float64 (-1) x
let gen_string x = gen__string (-1) x
let gen_bool x = gen__bool (-1) x
let gen_int x = gen__int (-1) x
let gen_float x = gen__float (-1) x
let gen_binary x = gen__binary (-1) x
let gen_uint x = gen__uint (-1) x
let gen_content x = gen__content (-1) x
let gen_document x = gen__document (-1) x
let gen_expression_type x = gen__expression_type (-1) x
let gen_expression x = gen__expression (-1) x
let gen_invoke x = gen__invoke (-1) x
let gen_param x = gen__param (-1) x
let gen_bitset x = gen__bitset (-1) x
let gen_state_type x = gen__state_type (-1) x
let gen_state x = gen__state (-1) x
let gen_transition_type x = gen__transition_type (-1) x
let gen_transition x = gen__transition (-1) x


let rec default_float64 () = 0.0
and default_string () = ""
and default_bool () = false
and default_int () = 0
and default_float () = default_float64 ()
and default_binary () = ""
and default_uint () = 0
and default_content () = `string (default_string ())
and default_document () =
  {
    Document.name = None;
    Document.states = [||];
    Document.transitions = [||];
  }
and default_expression_type () = `block
and default_expression () =
  {
    Expression.t = default_expression_type ();
    Expression.bool_val = None;
    Expression.int_val = None;
    Expression.float_val = None;
    Expression.string_val = None;
    Expression.args = [];
  }
and default_invoke () =
  {
    Invoke.t = None;
    Invoke.src = None;
    Invoke.id = None;
    Invoke.autoforward = parse_bool (Piqirun.parse_default "\b\000");
    Invoke.params = [||];
    Invoke.content = None;
    Invoke.on_exit = [||];
  }
and default_param () =
  {
    Param.id = default_string ();
    Param.expression = None;
  }
and default_bitset () = Bitset.make 1
and default_state_type () = `compound
and default_state () =
  {
    State.t = parse_state_type (Piqirun.parse_default "\b\001");
    State.idx = default_uint ();
    State.id = None;
    State.on_enter = [||];
    State.on_exit = [||];
    State.invocations = [||];
    State.data = [||];
    State.donedata = None;
    State.parent = default_uint ();
    State.children = default_bitset ();
    State.ancestors = default_bitset ();
    State.completion = default_bitset ();
    State.transitions = default_bitset ();
    State.has_history = parse_bool (Piqirun.parse_default "\b\000");
  }
and default_transition_type () = `external_
and default_transition () =
  {
    Transition.t = parse_transition_type (Piqirun.parse_default "\b\002");
    Transition.idx = default_uint ();
    Transition.source = default_uint ();
    Transition.events = [||];
    Transition.condition = None;
    Transition.on_transition = [||];
    Transition.targets = default_bitset ();
    Transition.conflicts = default_bitset ();
    Transition.exits = default_bitset ();
  }
