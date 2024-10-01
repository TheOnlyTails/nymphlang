import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/set
import nymph/ast/utils.{type Ident}

pub type Type {
  // Type declarations
  /// `int`
  IntType
  /// `float`
  FloatType
  /// `char`
  CharType
  /// `string`
  StringType
  /// `boolean`
  BooleanType
  /// `void`
  VoidType
  /// `never`
  NeverType
  /// `_`
  InferType
  /// `A + B`
  Intersection(left: Type, right: Type)
  /// `#[A]`
  ListType(item: Type)
  /// `#(A, B, C)`
  TupleType(members: List(Type))
  /// `#{ A: B }`
  MapType(key: Type, value: Type)
  /// `(A, B) -> C`
  FuncType(params: List(Type), return_type: Type)
  /// `A<B>`
  Reference(name: Ident, generics: List(GenericArg))
  /// `(A)`
  GroupedType(value: Type)

  // Internal types for resolving into
  StructType(members: set.Set(StructTypeMember), impls: List(StructImplId))
  GenericType(constraint: Option(Type), default: Option(Type))
}

pub fn simplify(it: Type) -> Type {
  case it {
    Intersection(left:, right:) ->
      case left == right {
        True -> simplify(left)
        False -> Intersection(simplify(left), simplify(right))
      }
    ListType(item:) -> ListType(simplify(item))
    TupleType(members:) -> TupleType(members |> list.map(simplify))
    MapType(key:, value:) -> MapType(simplify(key), simplify(value))
    FuncType(params:, return_type:) ->
      FuncType(params |> list.map(simplify), simplify(return_type))
    GenericType(constraint:, default:) ->
      GenericType(
        constraint |> option.map(simplify),
        default |> option.map(simplify),
      )
    GroupedType(value:) -> simplify(value)
    _ -> it
  }
}

pub fn is_assignable(from from: Type, to to: Type) -> Bool {
  case from, to {
    a, b if a == b -> True
    IntType, FloatType | CharType, IntType | _, InferType -> True
    GroupedType(a), b | a, GroupedType(b) -> is_assignable(a, b)
    GenericType(constraint:, ..), it ->
      constraint |> option.map(is_assignable(_, it)) |> option.unwrap(False)
    _, _ -> False
  }
}

pub type StructTypeMember =
  #(Ident, Type)

pub type StructImplId {
  StructImplId(name: Ident, generics: List(GenericArg))
}

pub type GenericArg {
  GenericArg(value: Type, name: Option(Ident))
}

pub type GenericParam {
  GenericParam(name: Ident, constraint: Option(Type), default: Option(Type))
}
