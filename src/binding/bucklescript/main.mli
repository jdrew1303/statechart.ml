type datamodels

type line_column = <
  line : int;
  column : int;
> Js.t

type message = <
  location : <
    start : line_column;
    finish : line_column;
  > Js.t;
  info : string;
> Js.t

val parse : Statechart_t.document -> datamodels -> <
  document : Statechart_t.document;
  errors : message array Js.Undefined.t;
> Js.t
val validate : Statechart_t.document ->  <
  warnings : message array Js.Undefined.t;
  errors : message array Js.Undefined.t;
> Js.t
val translate : Statechart_t.document -> Statechart_executable.document
val compile : Statechart_t.document -> datamodels -> <
  document : Statechart_executable.document Js.Undefined.t;
  warnings : message array Js.Undefined.t;
  errors : message array Js.Undefined.t;
> Js.t
