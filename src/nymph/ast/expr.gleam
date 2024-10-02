import gleam/dict
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import nymph/ast/operators
import nymph/ast/types.{type GenericArg, type GenericParam, type Type}
import nymph/ast/utils.{type Ident}

pub type Value {
  ModuleValue(Env)
  DataValue(type_: Type, mutable: Bool)
  TypeValue(Type)
}

pub type Env {
  Env(context: Option(Env), items: dict.Dict(String, Value))
}

pub type Statement {
  Expr(Expr)
  Let(name: Pattern, type_: Option(Type), mutable: Bool, value: Expr)
}

pub type Expr {
  Int(Int)
  Float(Float)
  Char(UtfCodepoint)
  String(List(StringPart))
  Boolean(Bool)
  Identifier(Ident)
  List(List(ListItem))
  Tuple(List(ListItem))
  Map(List(MapEntry))
  Struct(
    name: Ident,
    generics: List(GenericArg),
    fields: List(StructLiteralField),
  )
  Range(RangeKind)
  Call(func: Expr, generics: List(GenericArg), args: List(CallArg))
  MemberAccess(parent: Expr, member: Ident)
  IndexAccess(parent: Expr, index: Expr)
  Closure(
    params: List(ClosureParam),
    generics: List(GenericParam),
    return_type: Option(Type),
    body: Expr,
  )
  PrefixOp(op: operators.PrefixOperator, value: Expr)
  PostfixOp(op: operators.PostfixOperator, value: Expr)
  BinaryOp(lhs: Expr, op: operators.BinaryOperator, rhs: Expr)
  TypeOp(lhs: Expr, op: operators.TypeOperator, rhs: Type)
  AssignOp(lhs: Expr, op: operators.AssignOperator, rhs: Expr)
  Return(value: Option(Expr), label: Option(Ident))
  Break(value: Option(Expr), label: Option(Ident))
  Continue(label: Option(Ident))
  For(variable: Pattern, iterable: Expr, body: Expr, label: Option(Ident))
  While(condition: Expr, body: Expr, label: Option(Ident))
  If(condition: Expr, then: Expr, otherwise: Option(Expr))
  Match(value: Expr, arms: List(MatchArm))
  This
  Placeholder
  Block(body: List(Statement), label: Option(Ident))
  Grouped(value: Expr)
}

fn type_of(expr: Expr, env: Env) -> Result(Type, Nil) {
  case expr {
    Int(_) -> Ok(types.IntType)
    Float(_) -> Ok(types.FloatType)
    Char(_) -> Ok(types.CharType)
    String(_) -> Ok(types.StringType)
    Boolean(_) -> Ok(types.BooleanType)
    Identifier(name) -> dict.get(env.items, name) |> result.try(value_to_type)

    List([first, ..]) ->
      case first {
        ExprItem(it) -> type_of(it, env)
        SpreadItem(iterable) ->
          case type_of(iterable, env) {
            Ok(types.ListType(_)) as it -> it
            _ -> Error(Nil)
          }
      }

    Tuple(items) ->
      items
      |> list.try_fold([], fn(items, item) {
        case item {
          ExprItem(it) ->
            type_of(it, env) |> result.map(fn(it) { [it, ..items] })
          SpreadItem(it) ->
            case type_of(it, env) {
              Ok(types.TupleType(it)) ->
                Ok(list.concat([it |> list.reverse, items]))
              _ -> Error(Nil)
            }
        }
      })
      |> result.map(list.reverse)
      |> result.map(types.TupleType)

    Map([first, ..]) ->
      case first {
        ExprEntry(key, value) -> {
          use key <- result.try(type_of(key, env))
          use value <- result.try(type_of(value, env))
          Ok(types.MapType(key, value))
        }
        SpreadEntry(iterable) ->
          case type_of(iterable, env) {
            Ok(types.MapType(_, _)) as it -> it
            _ -> Error(Nil)
          }
      }

    Closure(params:, generics:, return_type:, body:) -> {
      let env =
        generics
        |> list.fold(env.items, fn(env, generic) {
          dict.insert(
            env,
            generic.name,
            TypeValue(types.GenericType(
              constraint: generic.constraint,
              default: generic.default,
            )),
          )
        })
        |> Env(context: env.context)

      use body_env <- result.try({
        use param_dicts <- result.try(
          list.try_map(params, fn(param) {
            pattern_to_members(param.name, param.type_, env)
          }),
        )
        let params =
          list.fold(param_dicts, dict.new(), fn(dict, new) {
            dict.merge(dict, new)
          })
        Ok(
          Env(
            ..env,
            items: dict.fold(params, env.items, fn(env, name, type_) {
              dict.insert(env, name, TypeValue(type_))
            }),
          ),
        )
      })

      use return_type <- result.try(case return_type {
        Some(it) -> Ok(it)
        None -> type_of(body, body_env)
      })

      Ok(types.FuncType(
        params: params |> list.map(fn(it) { it.type_ }),
        return_type:,
      ))
    }

    If(condition: _, then:, otherwise:) -> {
      case otherwise {
        Some(otherwise) -> {
          use then <- result.try(type_of(then, env))
          use otherwise <- result.try(type_of(otherwise, env))
          Ok(types.Intersection(then, otherwise))
        }
        None -> type_of(then, env)
      }
    }

    BinaryOp(lhs:, op:, rhs:) -> {
      use lhs <- result.try(type_of(lhs, env))
      use rhs <- result.try(type_of(rhs, env))

      case lhs, op, rhs {
        // +
        types.IntType, operators.Plus, types.IntType -> Ok(types.IntType)
        types.FloatType, operators.Plus, types.FloatType -> Ok(types.FloatType)
        types.StringType, operators.Plus, types.StringType ->
          Ok(types.FloatType)
        // -
        types.IntType, operators.Minus, types.IntType -> Ok(types.IntType)
        types.FloatType, operators.Minus, types.FloatType -> Ok(types.FloatType)
        // *
        types.IntType, operators.Times, types.IntType -> Ok(types.IntType)
        types.FloatType, operators.Times, types.FloatType -> Ok(types.FloatType)
        // /
        types.IntType, operators.Divide, types.IntType
        | types.FloatType, operators.Divide, types.FloatType
        -> Ok(types.FloatType)
        // %
        types.IntType, operators.Remainder, types.IntType -> Ok(types.IntType)
        // **
        types.IntType, operators.Power, types.IntType
        | types.IntType, operators.Power, types.FloatType
        | types.FloatType, operators.Power, types.IntType
        | types.FloatType, operators.Power, types.FloatType
        -> Ok(types.FloatType)
        // &, |, ^
        types.IntType, operators.BitAnd, types.IntType -> Ok(types.IntType)
        types.IntType, operators.BitOr, types.IntType -> Ok(types.IntType)
        types.IntType, operators.BitXor, types.IntType -> Ok(types.IntType)
        // ==, !=, <, >, <=, >=, in, !in
        _, operators.Equals, _
        | _, operators.NotEquals, _
        | _, operators.LessThan, _
        | _, operators.GreaterThan, _
        | _, operators.LessThanEquals, _
        | _, operators.GreaterThanEquals, _
        | _, operators.In, _
        | _, operators.NotIn, _
        -> Ok(types.BooleanType)
        // &&, ||
        types.BooleanType, operators.BoolAnd, types.BooleanType ->
          Ok(types.BooleanType)
        types.BooleanType, operators.BoolOr, types.BooleanType ->
          Ok(types.BooleanType)
        // |>
        _, operators.Pipe, types.FuncType(_, return_type:) -> Ok(return_type)
        _, _, _ -> Error(Nil)
      }
    }

    PostfixOp(op:, value:) ->
      type_of(value, env)
      |> result.try(fn(value) {
        case op, value {
          operators.Increment, types.IntType -> Ok(types.IntType)
          operators.Increment, types.FloatType -> Ok(types.FloatType)
          operators.Decrement, types.IntType -> Ok(types.IntType)
          operators.Decrement, types.FloatType -> Ok(types.FloatType)
          _, _ -> Error(Nil)
        }
      })

    PrefixOp(op:, value:) ->
      type_of(value, env)
      |> result.try(fn(value) {
        case op, value {
          operators.Not, types.BooleanType -> Ok(types.BooleanType)
          operators.Negate, types.IntType -> Ok(types.IntType)
          operators.Negate, types.FloatType -> Ok(types.FloatType)
          _, _ -> Error(Nil)
        }
      })

    TypeOp(op:, rhs:, ..) ->
      case op {
        operators.As -> Ok(rhs)
        operators.Is | operators.NotIs -> Ok(types.BooleanType)
      }

    Return(value:, label: _) | Break(value:, label: _) ->
      case value {
        Some(it) -> type_of(it, env)
        None -> Ok(types.VoidType)
      }

    Block(body:, label: _) ->
      list.try_fold(body, #(types.VoidType, env), fn(it, line) {
        let #(_, env) = it
        case line {
          Expr(it) -> type_of(it, env) |> result.map(fn(it) { #(it, env) })
          Let(name:, mutable:, type_:, value:) -> {
            use inferred_type <- result.try(case type_ {
              Some(it) -> Ok(it)
              None -> type_of(value, env)
            })
            use destructured_names <- result.try(pattern_to_members(
              name,
              inferred_type,
              env,
            ))

            Ok(#(
              inferred_type,
              Env(
                ..env,
                items: dict.merge(
                  env.items,
                  destructured_names
                    |> dict.map_values(fn(_, type_) {
                      DataValue(type_:, mutable:)
                    }),
                ),
              ),
            ))
          }
        }
      })
      |> result.map(fn(it) { types.simplify(it.0) })

    Grouped(it) -> type_of(it, env)

    AssignOp(_, _, _) | Continue(_) | Placeholder -> Ok(types.VoidType)
    Call(func:, generics:, args:) -> todo
    For(variable:, iterable:, body:, label: _) -> todo
    IndexAccess(parent:, index:) -> todo
    List(items) -> todo
    Map(entries) -> todo
    Match(value:, arms:) -> todo
    MemberAccess(parent:, member:) -> todo
    Range(kind) ->
      case kind {
        Exclusive(min:, max:) -> {
          use min <- result.try(type_of(min, env))
          use max <- result.try(type_of(max, env))
          Ok(
            types.Reference("RangeExclusive", [
              types.GenericArg(name: Some("Min"), value: min),
              types.GenericArg(name: Some("Max"), value: max),
            ]),
          )
        }
        Full -> Ok(types.Reference("RangeFull", []))
        Inclusive(min:, max:) -> {
          use min <- result.try(type_of(min, env))
          use max <- result.try(type_of(max, env))
          Ok(
            types.Reference("RangeExclusive", [
              types.GenericArg(name: Some("Min"), value: min),
              types.GenericArg(name: Some("Max"), value: max),
            ]),
          )
        }
      }
    Struct(name:, generics:, fields:) -> todo
    This ->
      env.context
      |> option.to_result(Nil)
      |> result.map(fn(it) {
        types.StructType(
          members: it.items
          |> list.map(fn(val) { todo }),
        )
      })
    While(body:, ..) -> todo
  }
}

fn value_to_type(from: Value) -> Result(Type, Nil) {
  case from {
    TypeValue(it) | DataValue(it, mutable: _) -> Ok(it)
    ModuleValue(_) -> Error(Nil)
  }
}

pub type StringPart {
  Grapheme(UtfCodepoint)
  EscapeSequence(EscapeSequence)
  Unicode(UtfCodepoint)
  InterpolatedExpr(Expr)
}

pub type EscapeSequence {
  Backslash
  Newline
  Carriage
  Tab
  Interpolation
  Apostrophe
  Quote
}

pub type ListItem {
  ExprItem(value: Expr)
  SpreadItem(iterable: Expr)
}

pub type MapEntry {
  ExprEntry(key: Expr, value: Expr)
  SpreadEntry(iterable: Expr)
}

pub type StructLiteralField {
  NamedField(name: Ident, value: Expr)
  ShorthandField(name: Ident)
  SpreadField(iterable: Expr)
}

pub type ClosureParam {
  ClosureParam(name: Pattern, type_: Type, default: Option(Expr), spread: Bool)
}

pub type RangeKind {
  Full
  Exclusive(min: Expr, max: Expr)
  Inclusive(min: Expr, max: Expr)
}

pub type CallArg {
  CallArg(value: Expr, name: Option(Ident), spread: Bool)
}

pub type MatchArm {
  MatchArm(pattern: Pattern, guard: Option(Expr), body: Expr)
}

pub type Pattern {
  IntPattern(Int)
  FloatPattern(Float)
  CharPattern(UtfCodepoint)
  StringPattern(List(StringPart))
  BooleanPattern(Bool)
  IdentifierPattern(Ident)
  ListPattern(List(ListPatternEntry))
  TuplePattern(List(ListPatternEntry))
  MapPattern(List(MapPatternEntry))
  TypePattern(Type)
  RangePattern(RangePatternKind)
  StructPattern(name: Ident, fields: List(StructPatternField))
  PlaceholderPattern
  RestPattern
  UnionPattern(left: Pattern, right: Pattern)
  GroupedPattern(value: Pattern)
}

fn pattern_to_members(
  pattern: Pattern,
  type_: Type,
  env: Env,
) -> Result(dict.Dict(String, Type), Nil) {
  case pattern {
    IntPattern(_)
    | FloatPattern(_)
    | CharPattern(_)
    | StringPattern(_)
    | BooleanPattern(_)
    | PlaceholderPattern -> Ok(dict.new())
    IdentifierPattern(it) -> Ok(dict.new() |> dict.insert(it, type_))
    ListPattern(it) ->
      case type_ {
        types.ListType(element_type) ->
          it
          |> list.try_map(fn(it) {
            case it {
              ListPatternEntry(pat) ->
                pattern_to_members(pat, element_type, env)
              ListPatternRest(name) ->
                Ok(
                  dict.new() |> dict.insert(name, types.ListType(element_type)),
                )
            }
          })
          |> result.map(fn(it) { list.fold(it, dict.new(), dict.merge) })
        _ -> Error(Nil)
      }
    TuplePattern(it) ->
      case type_ {
        types.TupleType(elements) -> {
          let elements_size = list.length(elements)
          it
          |> list.index_map(fn(element, i) {
            case element {
              ListPatternEntry(pat) ->
                pattern_to_members(pat, element_type, env)
              ListPatternRest(name) -> {
                let subset = elements |> list.drop(i - 1)
                Ok(dict.new() |> dict.insert(name, types.TupleType()))
              }
            }
          })
          |> result.map(fn(it) { list.fold(it, dict.new(), dict.merge) })
        }
        _ -> Error(Nil)
      }
    MapPattern(List(MapPatternEntry)) -> todo
    TypePattern -> todo
    RangePattern(kind) -> todo
    StructPattern(name: Ident, fields: List(StructPatternField)) -> todo
    RestPattern -> todo
    UnionPattern(left:, right:) -> todo
    GroupedPattern(value:) -> pattern_to_members(value, type_, env)
  }
}

pub type RangePatternKind {
  ExclusivePatternMin(min: Pattern)
  ExclusivePatternBoth(min: Pattern, max: Pattern)
  InclusivePatternMax(max: Pattern)
  InclusivePatternBoth(min: Pattern, max: Pattern)
}

pub type StructPatternField {
  NamedFieldPattern(name: Ident, value: Option(Pattern))
  RestFieldPattern
}

pub type ListPatternEntry {
  ListPatternEntry(value: Pattern)
  ListPatternRest(ident: Ident)
}

pub type MapPatternEntry {
  MapPatternEntry(key: Pattern, value: Pattern)
  MapPatternRest(ident: Ident)
}
