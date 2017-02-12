open Statechart_executable
open Statechart_datamodel
open Parser_flow
open Parser_common
module Ast = Spider_monkey_ast
module Err = Parse_error

let translate_binary_operator o =
  match o with
  | Ast.Expression.Binary.Equal -> `equal
  | Ast.Expression.Binary.NotEqual -> `not_equal
  | _ -> `int

let rec translate_expression expr =
  let _loc, expr = expr in
  match expr with
  | Ast.Expression.Binary a -> translate_binary a
  | _ -> null

and translate_binary a =
  let op = translate_binary_operator a.Ast.Expression.Binary.operator in
  let left = translate_expression a.Ast.Expression.Binary.left in
  let right = translate_expression a.Ast.Expression.Binary.right in
  complex_expr op [left; right]

let translate_statement statement =
  let _loc, statement = statement in
  match statement with
  | Ast.Statement.Expression e ->
    translate_expression e.Ast.Statement.Expression.expression
  | _ -> null

let translate_program ast =
  let loc, statements, _comments = ast in
  let statements = List.map translate_statement statements in
  complex_expr `block statements

let translate_errors errors =
  List.map (fun error ->
    let loc, err = error in
    let start = loc.Loc.start.Loc.line, loc.Loc.start.Loc.column in
    let finish = loc.Loc._end.Loc.line, loc.Loc._end.Loc.column in
    (start, finish), (Err.PP.error err)
  ) errors

let parse content =
  match parse_program false None content with
  | ast, [] -> Program (translate_program ast)
  | _, errors -> Error (translate_errors errors)
