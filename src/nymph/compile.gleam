//// All names are prefixed with $$ to avoid clashing with JS keywords

import gleam/bool
import gleam/dict.{type Dict}
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import nymph/ast/declaration
import nymph/ast/expr
import nymph/ast/module

pub type CompileOutput {
  CompileOutput(path: String, js: String, env: expr.Env)
}

pub fn compile(module: module.Module) -> CompileOutput {
  let #(env, js) =
    module.members |> list.map_fold(from: dict.new(), with: compile_member)

  CompileOutput(
    module.path,
    js
      |> list.prepend("import * as $ from 'prelude.mjs'")
      |> string.join("\n"),
    env,
  )
}

fn compile_member(
  env: expr.Env,
  member: declaration.Declaration,
) -> #(expr.Env, String) {
  case member {
    declaration.Import(path:, idents:) -> compile_import(env, path, idents)
    declaration.Let(meta:, value:) -> compile_let(env, meta, value)
    _ -> todo
  }
}

fn compile_import(
  env: expr.Env,
  path: List(String),
  idents: Option(Dict(String, Option(String))),
) -> #(expr.Env, String) {
  let module_name = path |> list.last |> result.unwrap("")

  let env =
    dict.combine(
      env,
      [
        case idents {
          None -> #(module_name, expr.ModuleValue(todo))
          Some(idents) -> todo
        },
      ]
        |> dict.from_list,
      fn(_, it) { it },
    )

  let js =
    case idents {
      None -> "import * as " <> module_name
      Some(idents) ->
        "import { "
        <> idents
        |> dict.to_list
        |> list.map(fn(entry) {
          let #(name, alias) = entry
          alias
          |> option.map(string.append(to: " as $$", suffix: _))
          |> option.unwrap("")
          |> string.append(to: "$$" <> name, suffix: _)
        })
        |> string.join(", ")
        <> " }"
    }
    <> " from \""
    <> path |> string.join("/")
    <> ".mjs\";"

  #(env, js)
}

fn compile_let(
  env: expr.Env,
  meta: declaration.LetDeclaration,
  value: expr.Expr,
) -> #(expr.Env, String) {
  let declaration.LetDeclaration(
    external:,
    mutable:,
    name:,
    visibility:,
    type_:,
  ) = meta
  let export = case visibility {
    Some(declaration.Private) -> ""
    None | Some(_) -> "export "
  }
  let keyword = case mutable {
    True -> "let "
    False -> "const "
  }

  let #(env, js) = compile_expr(env, value)
  #(dict.insert(env, name), export <> keyword <> name <> " = " <> js)
}

fn compile_expr(env: expr.Env, expr: expr.Expr) -> #(expr.Env, String) {
  let js = case expr {
    expr.Int(val) -> int.to_string(val)
    expr.Float(val) -> float.to_string(val)
    expr.Boolean(val) -> bool.to_string(val) |> string.lowercase
    // strings and chars are both represented as strings under the hood!
    expr.Char(val) ->
      "`\\u" <> string.utf_codepoint_to_int(val) |> int.to_base16 <> "`"
    expr.String(parts) ->
      "`"
      <> parts
      |> list.map(fn(part) {
        case part {
          expr.Grapheme(codepoint) | expr.Unicode(codepoint) ->
            "\\u" <> string.utf_codepoint_to_int(codepoint) |> int.to_base16
          expr.EscapeSequence(expr.Backslash) -> "\\"
          expr.EscapeSequence(expr.Newline) -> "\n"
          expr.EscapeSequence(expr.Carriage) -> "\r"
          expr.EscapeSequence(expr.Tab) -> "\t"
          expr.EscapeSequence(expr.Interpolation) -> "\\${"
          expr.EscapeSequence(expr.Quote) -> "\""
          expr.EscapeSequence(expr.Apostrophe) -> "'"
          expr.InterpolatedExpr(expr) ->
            "${" <> compile_expr(env, expr).1 <> "}"
        }
      })
      |> string.join("")
      <> "`"
    expr.Identifier(name) -> name
    // lists and tuples are both represented as lists under the hood!
    expr.List(items) | expr.Tuple(items) ->
      "["
      <> items
      |> list.map(fn(item) {
        case item {
          expr.ExprItem(it) -> compile_expr(env, it).1
          expr.SpreadItem(it) -> "...(" <> compile_expr(env, it).1 <> ")"
        }
      })
      |> string.join(",")
      <> "]"
    expr.Map(entries) ->
      "new Map(["
      <> entries
      |> list.map(fn(entry) {
        case entry {
          expr.ExprEntry(key, value) ->
            "["
            <> compile_expr(env, key).1
            <> ","
            <> compile_expr(env, value).1
            <> "]"
          expr.SpreadEntry(it) -> "...(" <> compile_expr(env, it).1 <> ")"
        }
      })
      |> string.join(",")
      <> "]"
    expr.Range(expr.Full) -> "$.RangeFull"
    expr.Range(expr.Exclusive(expr.Placeholder, max)) -> todo
    expr.Range(expr.Exclusive(min, expr.Placeholder)) -> todo
    expr.Range(expr.Exclusive(min, max)) -> todo
    expr.Range(expr.Inclusive(expr.Placeholder, max)) -> todo
    expr.Range(expr.Inclusive(min, expr.Placeholder)) -> todo
    expr.Range(expr.Inclusive(min, max)) -> todo
    expr.Call(func:, args:, generics: _) ->
      "new $.Func("
      <> compile_expr(func)
      <> ").call({"
      <> args
      |> list.map(fn(arg) {
        let expr.CallArg(value:, name:, spread:) = arg
      })
      <> "})"
    _ -> todo
  }

  #(env, js)
}
