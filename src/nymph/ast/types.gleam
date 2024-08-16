import gleam/option.{type Option}
import nymph/ast/utils.{type Ident}

pub type Type {
  IntType
  FloatType
  CharType
  StringType
  BooleanType
  VoidType
  NeverType
  InferType
  Intersection(left: Type, right: Type)
  ListType(item: Type)
  TupleType(members: List(Type))
  MapType(key: Type, value: Type)
  Func(params: List(Type), return: Type)
  Reference(name: Ident, generics: List(GenericArg))
  GroupedType(value: Type)
}

pub type GenericArg {
  GenericArg(value: Type, name: Option(Ident))
}

pub type GenericParam {
  GenericParam(name: Ident, constraint: Option(Type), default: Option(Type))
}
