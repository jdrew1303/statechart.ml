open Statechart
module Piqirun = Statechart_format_runtime

let rec parse_float64 x = Piqirun.float_of_fixed64 x
and packed_parse_float64 x = Piqirun.float_of_packed_fixed64 x

and parse_uint x = Piqirun.int_of_varint x
and packed_parse_uint x = Piqirun.int_of_packed_varint x

and parse_string x = Piqirun.string_of_block x

and parse_bool x = Piqirun.bool_of_varint x
and packed_parse_bool x = Piqirun.bool_of_packed_varint x

and parse_int x = Piqirun.int_of_zigzag_varint x
and packed_parse_int x = Piqirun.int_of_packed_zigzag_varint x

and parse_float x = parse_float64 x
and packed_parse_float x = packed_parse_float64 x

and parse_binary x = Piqirun.string_of_block x

and parse_ref x = parse_uint x
and packed_parse_ref x = packed_parse_uint x

and parse_binding x =
  match Piqirun.int32_of_signed_varint x with
    | 1l -> `early
    | 2l -> `late
    | x -> Piqirun.error_enum_const x
and packed_parse_binding x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 1l -> `early
    | 2l -> `late
    | x -> Piqirun.error_enum_const x

and parse_document x =
  let x = Piqirun.parse_record x in
  let _name, x = Piqirun.parse_optional_field 1 parse_string x in
  let _binding, x = Piqirun.parse_required_field 2 parse_binding x ~default:"\b\001" in
  let _datamodel, x = Piqirun.parse_repeated_field 3 parse_param x in
  let _initial_transitions, x = Piqirun.parse_repeated_field 4 parse_transition x in
  let _states, x = Piqirun.parse_repeated_field 5 parse_state x in
  Piqirun.check_unparsed_fields x;
  {
    Document.name = _name;
    Document.binding = _binding;
    Document.datamodel = _datamodel;
    Document.initial_transitions = _initial_transitions;
    Document.states = _states;
  }

and parse_state_type x =
  match Piqirun.int32_of_signed_varint x with
    | 1l -> `composite
    | 2l -> `basic
    | 3l -> `parallel
    | 4l -> `history
    | 5l -> `initial
    | 6l -> `final
    | x -> Piqirun.error_enum_const x
and packed_parse_state_type x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 1l -> `composite
    | 2l -> `basic
    | 3l -> `parallel
    | 4l -> `history
    | 5l -> `initial
    | 6l -> `final
    | x -> Piqirun.error_enum_const x

and parse_state x =
  let x = Piqirun.parse_record x in
  let _idx, x = Piqirun.parse_required_field 1 parse_ref x in
  let _depth, x = Piqirun.parse_required_field 2 parse_uint x in
  let _priority, x = Piqirun.parse_required_field 3 parse_uint x in
  let _id, x = Piqirun.parse_optional_field 4 parse_string x in
  let _type_, x = Piqirun.parse_required_field 5 parse_state_type x ~default:"\b\001" in
  let _initial_states, x = Piqirun.parse_repeated_field 6 parse_ref x in
  let _transitions, x = Piqirun.parse_repeated_field 7 parse_transition x in
  let _invocations, x = Piqirun.parse_repeated_field 8 parse_invoke x in
  let _on_enter, x = Piqirun.parse_repeated_field 9 parse_expression x in
  let _on_exit, x = Piqirun.parse_repeated_field 10 parse_expression x in
  let _children, x = Piqirun.parse_repeated_field 11 parse_ref x in
  let _parent, x = Piqirun.parse_optional_field 12 parse_ref x in
  let _ancestors, x = Piqirun.parse_repeated_field 13 parse_ref x in
  let _descendants, x = Piqirun.parse_repeated_field 14 parse_ref x in
  let _history, x = Piqirun.parse_optional_field 15 parse_ref x in
  let _history_type, x = Piqirun.parse_optional_field 16 parse_history_type x in
  Piqirun.check_unparsed_fields x;
  {
    State.idx = _idx;
    State.depth = _depth;
    State.priority = _priority;
    State.id = _id;
    State.type_ = _type_;
    State.initial_states = _initial_states;
    State.transitions = _transitions;
    State.invocations = _invocations;
    State.on_enter = _on_enter;
    State.on_exit = _on_exit;
    State.children = _children;
    State.parent = _parent;
    State.ancestors = _ancestors;
    State.descendants = _descendants;
    State.history = _history;
    State.history_type = _history_type;
  }

and parse_history_type x =
  match Piqirun.int32_of_signed_varint x with
    | 1l -> `shallow
    | 2l -> `deep
    | x -> Piqirun.error_enum_const x
and packed_parse_history_type x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 1l -> `shallow
    | 2l -> `deep
    | x -> Piqirun.error_enum_const x

and parse_transition_type x =
  match Piqirun.int32_of_signed_varint x with
    | 1l -> `external_
    | 2l -> `internal
    | x -> Piqirun.error_enum_const x
and packed_parse_transition_type x =
  match Piqirun.int32_of_packed_signed_varint x with
    | 1l -> `external_
    | 2l -> `internal
    | x -> Piqirun.error_enum_const x

and parse_transition x =
  let x = Piqirun.parse_record x in
  let _scope, x = Piqirun.parse_required_field 1 parse_ref x in
  let _depth, x = Piqirun.parse_required_field 2 parse_uint x in
  let _priority, x = Piqirun.parse_required_field 3 parse_uint x in
  let _source, x = Piqirun.parse_optional_field 4 parse_ref x in
  let _targets, x = Piqirun.parse_repeated_field 5 parse_ref x in
  let _events, x = Piqirun.parse_repeated_field 6 parse_string x in
  let _condition, x = Piqirun.parse_optional_field 7 parse_expression x in
  let _type_, x = Piqirun.parse_required_field 8 parse_transition_type x ~default:"\b\001" in
  let _on_transition, x = Piqirun.parse_repeated_field 9 parse_expression x in
  Piqirun.check_unparsed_fields x;
  {
    Transition.scope = _scope;
    Transition.depth = _depth;
    Transition.priority = _priority;
    Transition.source = _source;
    Transition.targets = _targets;
    Transition.events = _events;
    Transition.condition = _condition;
    Transition.type_ = _type_;
    Transition.on_transition = _on_transition;
  }

and parse_invoke x =
  let x = Piqirun.parse_record x in
  let _type_, x = Piqirun.parse_optional_field 1 parse_expression x in
  let _src, x = Piqirun.parse_optional_field 2 parse_expression x in
  let _id, x = Piqirun.parse_optional_field 3 parse_expression x in
  let _namelist, x = Piqirun.parse_repeated_field 4 parse_var x in
  let _autoforward, x = Piqirun.parse_required_field 5 parse_bool x ~default:"\b\000" in
  let _params, x = Piqirun.parse_repeated_field 6 parse_param x in
  let _content, x = Piqirun.parse_optional_field 7 parse_content x in
  let _on_exit, x = Piqirun.parse_repeated_field 8 parse_expression x in
  Piqirun.check_unparsed_fields x;
  {
    Invoke.type_ = _type_;
    Invoke.src = _src;
    Invoke.id = _id;
    Invoke.namelist = _namelist;
    Invoke.autoforward = _autoforward;
    Invoke.params = _params;
    Invoke.content = _content;
    Invoke.on_exit = _on_exit;
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

and parse_content x =
  let code, x = Piqirun.parse_variant x in
  match code with
    | 1 ->
        let res = parse_string x in
        `string res
    | 2 -> (parse_expression x :> Statechart.content)
    | 3 ->
        let res = parse_document x in
        `document res
    | _ -> Piqirun.error_variant x code

and parse_expression x =
  let code, x = Piqirun.parse_variant x in
  match code with
    | 1 ->
        let res = parse_bool x in
        `bool res
    | 2 ->
        let res = parse_int x in
        `int res
    | 3 ->
        let res = parse_float x in
        `float res
    | 4 ->
        let res = parse_string x in
        `string res
    | 5 ->
        let res = parse_binary x in
        `binary res
    | 6 ->
        let res = parse_assign x in
        `assign res
    | 7 ->
        let res = parse_foreach x in
        `foreach res
    | 8 ->
        let res = parse_case x in
        `case res
    | 9 ->
        let res = parse_log x in
        `log res
    | 10 ->
        let res = parse_raise x in
        `raise res
    | 11 ->
        let res = parse_var x in
        `var res
    | _ -> Piqirun.error_variant x code

and parse_assign x =
  let x = Piqirun.parse_record x in
  let _id, x = Piqirun.parse_required_field 1 parse_string x in
  let _expression, x = Piqirun.parse_optional_field 2 parse_expression x in
  Piqirun.check_unparsed_fields x;
  {
    Assign.id = _id;
    Assign.expression = _expression;
  }

and parse_foreach x =
  let x = Piqirun.parse_record x in
  let _array, x = Piqirun.parse_required_field 1 parse_expression x in
  let _item, x = Piqirun.parse_optional_field 2 parse_var x in
  let _index, x = Piqirun.parse_optional_field 3 parse_var x in
  let _expressions, x = Piqirun.parse_repeated_field 4 parse_expression x in
  Piqirun.check_unparsed_fields x;
  {
    Foreach.array = _array;
    Foreach.item = _item;
    Foreach.index = _index;
    Foreach.expressions = _expressions;
  }

and parse_case x =
  let x = Piqirun.parse_record x in
  let _clauses, x = Piqirun.parse_repeated_field 1 parse_case_clause x in
  Piqirun.check_unparsed_fields x;
  {
    Case.clauses = _clauses;
  }

and parse_case_clause x =
  let x = Piqirun.parse_record x in
  let _clause, x = Piqirun.parse_required_field 1 parse_expression x in
  let _body, x = Piqirun.parse_required_field 2 parse_expression x in
  Piqirun.check_unparsed_fields x;
  {
    Case_clause.clause = _clause;
    Case_clause.body = _body;
  }

and parse_log x =
  let x = Piqirun.parse_record x in
  let _label, x = Piqirun.parse_optional_field 1 parse_string x in
  let _expression, x = Piqirun.parse_optional_field 2 parse_expression x in
  Piqirun.check_unparsed_fields x;
  {
    Log.label = _label;
    Log.expression = _expression;
  }

and parse_raise x =
  let x = Piqirun.parse_record x in
  let _event, x = Piqirun.parse_required_field 1 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Raise.event = _event;
  }

and parse_var x =
  let x = Piqirun.parse_record x in
  let _name, x = Piqirun.parse_required_field 1 parse_string x in
  Piqirun.check_unparsed_fields x;
  {
    Var.name = _name;
  }


let rec gen__float64 code x = Piqirun.float_to_fixed64 code x
and packed_gen__float64 x = Piqirun.float_to_packed_fixed64 x

and gen__uint code x = Piqirun.int_to_varint code x
and packed_gen__uint x = Piqirun.int_to_packed_varint x

and gen__string code x = Piqirun.string_to_block code x

and gen__bool code x = Piqirun.bool_to_varint code x
and packed_gen__bool x = Piqirun.bool_to_packed_varint x

and gen__int code x = Piqirun.int_to_zigzag_varint code x
and packed_gen__int x = Piqirun.int_to_packed_zigzag_varint x

and gen__float code x = gen__float64 code x
and packed_gen__float x = packed_gen__float64 x

and gen__binary code x = Piqirun.string_to_block code x

and gen__ref code x = gen__uint code x
and packed_gen__ref x = packed_gen__uint x

and gen__binding code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `early -> 1l
    | `late -> 2l
  )
and packed_gen__binding x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `early -> 1l
    | `late -> 2l
  )

and gen__document code x =
  let _name = Piqirun.gen_optional_field 1 gen__string x.Document.name in
  let _binding = Piqirun.gen_required_field 2 gen__binding x.Document.binding in
  let _datamodel = Piqirun.gen_repeated_field 3 gen__param x.Document.datamodel in
  let _initial_transitions = Piqirun.gen_repeated_field 4 gen__transition x.Document.initial_transitions in
  let _states = Piqirun.gen_repeated_field 5 gen__state x.Document.states in
  Piqirun.gen_record code (_name :: _binding :: _datamodel :: _initial_transitions :: _states :: [])

and gen__state_type code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `composite -> 1l
    | `basic -> 2l
    | `parallel -> 3l
    | `history -> 4l
    | `initial -> 5l
    | `final -> 6l
  )
and packed_gen__state_type x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `composite -> 1l
    | `basic -> 2l
    | `parallel -> 3l
    | `history -> 4l
    | `initial -> 5l
    | `final -> 6l
  )

and gen__state code x =
  let _idx = Piqirun.gen_required_field 1 gen__ref x.State.idx in
  let _depth = Piqirun.gen_required_field 2 gen__uint x.State.depth in
  let _priority = Piqirun.gen_required_field 3 gen__uint x.State.priority in
  let _id = Piqirun.gen_optional_field 4 gen__string x.State.id in
  let _type_ = Piqirun.gen_required_field 5 gen__state_type x.State.type_ in
  let _initial_states = Piqirun.gen_repeated_field 6 gen__ref x.State.initial_states in
  let _transitions = Piqirun.gen_repeated_field 7 gen__transition x.State.transitions in
  let _invocations = Piqirun.gen_repeated_field 8 gen__invoke x.State.invocations in
  let _on_enter = Piqirun.gen_repeated_field 9 gen__expression x.State.on_enter in
  let _on_exit = Piqirun.gen_repeated_field 10 gen__expression x.State.on_exit in
  let _children = Piqirun.gen_repeated_field 11 gen__ref x.State.children in
  let _parent = Piqirun.gen_optional_field 12 gen__ref x.State.parent in
  let _ancestors = Piqirun.gen_repeated_field 13 gen__ref x.State.ancestors in
  let _descendants = Piqirun.gen_repeated_field 14 gen__ref x.State.descendants in
  let _history = Piqirun.gen_optional_field 15 gen__ref x.State.history in
  let _history_type = Piqirun.gen_optional_field 16 gen__history_type x.State.history_type in
  Piqirun.gen_record code (_idx :: _depth :: _priority :: _id :: _type_ :: _initial_states :: _transitions :: _invocations :: _on_enter :: _on_exit :: _children :: _parent :: _ancestors :: _descendants :: _history :: _history_type :: [])

and gen__history_type code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `shallow -> 1l
    | `deep -> 2l
  )
and packed_gen__history_type x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `shallow -> 1l
    | `deep -> 2l
  )

and gen__transition_type code x =
  Piqirun.int32_to_signed_varint code (match x with
    | `external_ -> 1l
    | `internal -> 2l
  )
and packed_gen__transition_type x =
  Piqirun.int32_to_packed_signed_varint (match x with
    | `external_ -> 1l
    | `internal -> 2l
  )

and gen__transition code x =
  let _scope = Piqirun.gen_required_field 1 gen__ref x.Transition.scope in
  let _depth = Piqirun.gen_required_field 2 gen__uint x.Transition.depth in
  let _priority = Piqirun.gen_required_field 3 gen__uint x.Transition.priority in
  let _source = Piqirun.gen_optional_field 4 gen__ref x.Transition.source in
  let _targets = Piqirun.gen_repeated_field 5 gen__ref x.Transition.targets in
  let _events = Piqirun.gen_repeated_field 6 gen__string x.Transition.events in
  let _condition = Piqirun.gen_optional_field 7 gen__expression x.Transition.condition in
  let _type_ = Piqirun.gen_required_field 8 gen__transition_type x.Transition.type_ in
  let _on_transition = Piqirun.gen_repeated_field 9 gen__expression x.Transition.on_transition in
  Piqirun.gen_record code (_scope :: _depth :: _priority :: _source :: _targets :: _events :: _condition :: _type_ :: _on_transition :: [])

and gen__invoke code x =
  let _type_ = Piqirun.gen_optional_field 1 gen__expression x.Invoke.type_ in
  let _src = Piqirun.gen_optional_field 2 gen__expression x.Invoke.src in
  let _id = Piqirun.gen_optional_field 3 gen__expression x.Invoke.id in
  let _namelist = Piqirun.gen_repeated_field 4 gen__var x.Invoke.namelist in
  let _autoforward = Piqirun.gen_required_field 5 gen__bool x.Invoke.autoforward in
  let _params = Piqirun.gen_repeated_field 6 gen__param x.Invoke.params in
  let _content = Piqirun.gen_optional_field 7 gen__content x.Invoke.content in
  let _on_exit = Piqirun.gen_repeated_field 8 gen__expression x.Invoke.on_exit in
  Piqirun.gen_record code (_type_ :: _src :: _id :: _namelist :: _autoforward :: _params :: _content :: _on_exit :: [])

and gen__param code x =
  let _id = Piqirun.gen_required_field 1 gen__string x.Param.id in
  let _expression = Piqirun.gen_optional_field 2 gen__expression x.Param.expression in
  Piqirun.gen_record code (_id :: _expression :: [])

and gen__content code (x:Statechart.content) =
  Piqirun.gen_record code [(match x with
    | `string x -> gen__string 1 x
    | (#Statechart.expression as x) -> gen__expression 2 x
    | `document x -> gen__document 3 x
  )]

and gen__expression code (x:Statechart.expression) =
  Piqirun.gen_record code [(match x with
    | `bool x -> gen__bool 1 x
    | `int x -> gen__int 2 x
    | `float x -> gen__float 3 x
    | `string x -> gen__string 4 x
    | `binary x -> gen__binary 5 x
    | `assign x -> gen__assign 6 x
    | `foreach x -> gen__foreach 7 x
    | `case x -> gen__case 8 x
    | `log x -> gen__log 9 x
    | `raise x -> gen__raise 10 x
    | `var x -> gen__var 11 x
  )]

and gen__assign code x =
  let _id = Piqirun.gen_required_field 1 gen__string x.Assign.id in
  let _expression = Piqirun.gen_optional_field 2 gen__expression x.Assign.expression in
  Piqirun.gen_record code (_id :: _expression :: [])

and gen__foreach code x =
  let _array = Piqirun.gen_required_field 1 gen__expression x.Foreach.array in
  let _item = Piqirun.gen_optional_field 2 gen__var x.Foreach.item in
  let _index = Piqirun.gen_optional_field 3 gen__var x.Foreach.index in
  let _expressions = Piqirun.gen_repeated_field 4 gen__expression x.Foreach.expressions in
  Piqirun.gen_record code (_array :: _item :: _index :: _expressions :: [])

and gen__case code x =
  let _clauses = Piqirun.gen_repeated_field 1 gen__case_clause x.Case.clauses in
  Piqirun.gen_record code (_clauses :: [])

and gen__case_clause code x =
  let _clause = Piqirun.gen_required_field 1 gen__expression x.Case_clause.clause in
  let _body = Piqirun.gen_required_field 2 gen__expression x.Case_clause.body in
  Piqirun.gen_record code (_clause :: _body :: [])

and gen__log code x =
  let _label = Piqirun.gen_optional_field 1 gen__string x.Log.label in
  let _expression = Piqirun.gen_optional_field 2 gen__expression x.Log.expression in
  Piqirun.gen_record code (_label :: _expression :: [])

and gen__raise code x =
  let _event = Piqirun.gen_required_field 1 gen__string x.Raise.event in
  Piqirun.gen_record code (_event :: [])

and gen__var code x =
  let _name = Piqirun.gen_required_field 1 gen__string x.Var.name in
  Piqirun.gen_record code (_name :: [])


let gen_float64 x = gen__float64 (-1) x
let gen_uint x = gen__uint (-1) x
let gen_string x = gen__string (-1) x
let gen_bool x = gen__bool (-1) x
let gen_int x = gen__int (-1) x
let gen_float x = gen__float (-1) x
let gen_binary x = gen__binary (-1) x
let gen_ref x = gen__ref (-1) x
let gen_binding x = gen__binding (-1) x
let gen_document x = gen__document (-1) x
let gen_state_type x = gen__state_type (-1) x
let gen_state x = gen__state (-1) x
let gen_history_type x = gen__history_type (-1) x
let gen_transition_type x = gen__transition_type (-1) x
let gen_transition x = gen__transition (-1) x
let gen_invoke x = gen__invoke (-1) x
let gen_param x = gen__param (-1) x
let gen_content x = gen__content (-1) x
let gen_expression x = gen__expression (-1) x
let gen_assign x = gen__assign (-1) x
let gen_foreach x = gen__foreach (-1) x
let gen_case x = gen__case (-1) x
let gen_case_clause x = gen__case_clause (-1) x
let gen_log x = gen__log (-1) x
let gen_raise x = gen__raise (-1) x
let gen_var x = gen__var (-1) x


let rec default_float64 () = 0.0
and default_uint () = 0
and default_string () = ""
and default_bool () = false
and default_int () = 0
and default_float () = default_float64 ()
and default_binary () = ""
and default_ref () = default_uint ()
and default_binding () = `early
and default_document () =
  {
    Document.name = None;
    Document.binding = parse_binding (Piqirun.parse_default "\b\001");
    Document.datamodel = [];
    Document.initial_transitions = [];
    Document.states = [];
  }
and default_state_type () = `composite
and default_state () =
  {
    State.idx = default_ref ();
    State.depth = default_uint ();
    State.priority = default_uint ();
    State.id = None;
    State.type_ = parse_state_type (Piqirun.parse_default "\b\001");
    State.initial_states = [];
    State.transitions = [];
    State.invocations = [];
    State.on_enter = [];
    State.on_exit = [];
    State.children = [];
    State.parent = None;
    State.ancestors = [];
    State.descendants = [];
    State.history = None;
    State.history_type = None;
  }
and default_history_type () = `shallow
and default_transition_type () = `external_
and default_transition () =
  {
    Transition.scope = default_ref ();
    Transition.depth = default_uint ();
    Transition.priority = default_uint ();
    Transition.source = None;
    Transition.targets = [];
    Transition.events = [];
    Transition.condition = None;
    Transition.type_ = parse_transition_type (Piqirun.parse_default "\b\001");
    Transition.on_transition = [];
  }
and default_invoke () =
  {
    Invoke.type_ = None;
    Invoke.src = None;
    Invoke.id = None;
    Invoke.namelist = [];
    Invoke.autoforward = parse_bool (Piqirun.parse_default "\b\000");
    Invoke.params = [];
    Invoke.content = None;
    Invoke.on_exit = [];
  }
and default_param () =
  {
    Param.id = default_string ();
    Param.expression = None;
  }
and default_content () = `string (default_string ())
and default_expression () = `bool (default_bool ())
and default_assign () =
  {
    Assign.id = default_string ();
    Assign.expression = None;
  }
and default_foreach () =
  {
    Foreach.array = default_expression ();
    Foreach.item = None;
    Foreach.index = None;
    Foreach.expressions = [];
  }
and default_case () =
  {
    Case.clauses = [];
  }
and default_case_clause () =
  {
    Case_clause.clause = default_expression ();
    Case_clause.body = default_expression ();
  }
and default_log () =
  {
    Log.label = None;
    Log.expression = None;
  }
and default_raise () =
  {
    Raise.event = default_string ();
  }
and default_var () =
  {
    Var.name = default_string ();
  }
