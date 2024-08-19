import gleam/dict.{type Dict}
import gleam/option.{type Option}
import nymph/ast/operators
import nymph/ast/types.{type GenericArg, type GenericParam, type Type}
import nymph/ast/utils.{type Ident}

pub type Statement {
  Expr(Expr)
  Let(name: Ident, type_: Type, mutable: Bool, value: Expr)
}

pub type Expr {
  Int(Int)
  Float(Float)
  Char(UtfCodepoint)
  String(List(StringPart))
  Boolean(Bool)
  Identifier(Ident)
  List(List(Expr))
  Tuple(List(Expr))
  Map(Dict(Expr, Expr))
  Struct(name: Ident, fields: List(StructLiteralField))
  Range(RangeKind)
  Call(func: Expr, generics: List(GenericArg), args: List(CallArg))
  MemberAccess(parent: Expr, member: Ident)
  IndexAccess(parent: Expr, index: Expr)
  Closure(
    params: List(FuncParam),
    generics: List(GenericParam),
    return: Option(Type),
    body: Expr,
  )
  PrefixOp(op: operators.PrefixOperator, value: Expr)
  PostfixOp(op: operators.PostfixOperator, value: Expr)
  BinaryOp(left: Expr, op: operators.BinaryOperator, right: Expr)
  TypeOp(left: Expr, op: operators.TypeOperator, right: Type)
  AssignOp(left: Expr, op: operators.AssignOperator, right: Expr)
  Return(value: Expr, label: Option(Ident))
  Break(value: Expr, label: Option(Ident))
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

pub type StructLiteralField {
  Named(name: Ident, value: Expr)
  Shorthand(name: Ident)
  Spread(iterable: Expr)
}

pub type FuncParam {
  FuncParam(name: Pattern, type_: Type, default: Option(Expr), spread: Bool)
}

pub type RangeKind {
  Exclusive(min: Option(Expr), max: Option(Expr))
  Inclusive(min: Option(Expr), max: Expr)
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
  ListPattern(List(Pattern))
  TuplePattern(List(Pattern))
  MapPattern(Dict(Pattern, Option(Pattern)))
  TypePattern(Type)
  RangePattern(RangePatternKind)
  StructPattern(name: Ident, fields: List(StructPatternField))
  PlaceholderPattern
  RestPattern
  DisjunctionPattern(left: Pattern, right: Pattern)
  GroupedPattern(value: Pattern)
}

pub type RangePatternKind {
  ExclusivePattern(min: Pattern, max: Option(Pattern))
  InclusivePattern(min: Option(Pattern), max: Pattern)
}

pub type StructPatternField {
  NamedField(name: Ident, pattern: Option(Pattern))
  Rest
}
