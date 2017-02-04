open Statechart
open Statechart_datamodel
open Parser_flow
open Parser_common
module Ast = Spider_monkey_ast
module Err = Parse_error

let default_expr = {
  Expression.t=`int;
  bool_val=None;
  int_val=Some 1;
  float_val=None;
  string_val=None;
  args=[];
}

let translate_binary_operator o =
  match o with
  | Ast.Expression.Binary.Equal -> `equal
  | Ast.Expression.Binary.NotEqual -> `not_equal
  | _ -> `int

let rec translate_expression expr =
  let _loc, expr = expr in
  match expr with
  | Ast.Expression.Binary a -> translate_binary a
  | _ -> default_expr

and translate_binary a =
  {
    Expression.t=translate_binary_operator a.Ast.Expression.Binary.operator;
    bool_val=None;
    int_val=None;
    float_val=None;
    string_val=None;
    args=[
      translate_expression a.Ast.Expression.Binary.left;
      translate_expression a.Ast.Expression.Binary.right;
    ];
  }

let translate_statement statement =
  let _loc, statement = statement in
  match statement with
  | Ast.Statement.Expression e -> translate_expression e.Ast.Statement.Expression.expression
  | _ -> default_expr

let translate_program ast =
  let loc, statements, _comments = ast in
  List.map translate_statement statements

let translate_errors errors =
  List.map (fun error ->
    let loc, err = error in
    loc.Loc.start.Loc.line, (Err.PP.error err)
  ) errors

let parse content =
  match parse_program false None content with
  | ast, [] -> Program (translate_program ast)
  | _, errors -> Error (translate_errors errors)
