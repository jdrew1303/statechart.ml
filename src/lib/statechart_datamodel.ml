type result = Program of Statechart_executable.expression
            | Error of (int * string) list

type parser = string -> result

let parse document datamodels =
  (* TODO *)
  document
