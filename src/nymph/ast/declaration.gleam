import nymph/ast/expr.{type Expr, type Pattern}
import nymph/ast/types.{type GenericArg, type GenericParam, type Type}
import nymph/ast/utils.{type Ident}
import gleam/dict.{type Dict}
import gleam/option.{type Option}

pub type Module {
  Module(members: List(Declaration))
}

pub type Declaration {
  /// An `import` declaration imports an external module into the current module,
  /// either from inside the project or from a published package.
  /// An optional `with` clause can be used to only import specific items from the module,
  /// and make them available without a module qualifier, or under different names.
  ///
  /// ```
  /// import std/math
  /// math.sin(math.pi) // 0
  ///
  /// import std/math with (sin as sine, cos as cosine, tan as tangent)
	/// ```
  Import(path: List(Ident), idents: Option(Dict(Ident, Option(Ident))))
  /// Redefines a type with a new name.
  /// ```
  /// type VeryVeryNested = #[#(#{#[int]: #(string, float)}, #[boolean)] // don't do this
  /// type TupleList<K, V> = #[#(K, V)]
  /// ```
  TypeAlias(meta: TypeAliasDeclaration, value: Type)
  /// An algebraic product type, with multiple named fields.
  /// After the fields, you may define variables and functions that can access those fields via the `this` object.
  ///
  /// The struct definition may also include a `namespace` block, which includes variables, functions, and type aliases,
  /// available from the type directly rather than an instance of the struct.
  ///
  /// You can implement interfaces for the struct by using an `impl` block,
  /// which also marks the struct as a subtype of the implemented interface.
  ///
  /// Two instances of an object are considered equal if:
  /// - They are both instances of the same struct.
  /// - The values of each of their fields are the same.
  ///
  /// ```
  /// struct Person {
  ///   name: string,
  ///   age: int
  ///
  ///   let first_name = name.split()[0]
  ///   let last_name = name.split()[1]
  ///
  ///   func is_minor() -> this.age < Person.age_of_majority
  ///
  ///   namespace {
  ///     let age_of_majority = 18
  ///   }
  /// }
  /// ```
  Struct(
    visibility: Option(Visibility),
    name: Ident,
    generics: List(GenericParam),
    fields: List(StructField),
    members: List(StructInnerMember),
  )
	/// An algebraic sum type, containing multiple named variants,
	/// each having an associated constructor and fields.
	/// 
	/// ```
	/// enum Option<T> {
	/// 	Some(T),
	/// 	None
	/// 
	/// 	func map<R>(f: (T) -> R) -> match this {
	/// 		Some(it) -> Some(f(it)),
	/// 		None -> None
	/// 	}
	/// }
	/// ```
  Enum(
    visibility: Option(Visibility),
    name: Ident,
    generics: List(GenericParam),
    variants: List(EnumVariant),
    members: List(StructInnerMember),
  )
  Interface(
    visibility: Option(Visibility),
    name: Ident,
    generics: List(GenericParam),
    members: List(InterfaceMember),
    blocks: List(InterfaceInnerBlock),
  )
  /// An extension `impl` block extends a declaration with custom variables, functions, and types.
  ImplExtension(ImplExtensionDeclaration)
  /// An interface `impl` block extends a declaration using an interface.
  /// For example:
  /// ```
  /// struct Person {
  ///   name: string,
  ///   age: int,
  /// }
  ///
  /// impl Comparable<Person> for Person {
  ///   func compare(other: Person) -> this.age.compare(other.age)
  /// }
  /// ```
  ImplFor(
    visibility: Option(Visibility),
    generics: List(GenericParam),
    super_name: Ident,
    super_generics: List(GenericArg),
    type_: Type,
    members: List(ImplMember),
  )
  Namespace(
    visibility: Option(Visibility),
    name: Ident,
    generics: List(GenericParam),
    members: List(ImplMember),
  )
  Let(meta: LetDeclaration, value: Expr)
  Func(meta: FuncDeclaration, body: Expr)
}

pub type Visibility {
  Public
  Internal
  Private
}

pub type TypeAliasDeclaration {
  TypeAliasDeclaration(
    visibility: Option(Visibility),
    name: Ident,
    generics: List(GenericParam),
  )
}

pub type LetDeclaration {
  LetDeclaration(
    visibility: Option(Visibility),
    mutable: Bool,
    name: Ident,
    type_: Option(Type),
  )
}

pub type FuncDeclaration {
  FuncDeclaration(
    visibility: Option(Visibility),
    name: Ident,
    generics: List(GenericParam),
    params: List(FuncParam),
    return_type: Option(Type),
  )
}

pub type ImplExtensionDeclaration {
  ImplExtensionDeclaration(
    visibility: Option(Visibility),
    name: Ident,
    generics: List(GenericParam),
    members: List(ImplMember),
  )
}

pub type StructField {
  StructField(
    visibility: Option(Visibility),
    mutable: Bool,
    name: Ident,
    type_: Type,
    default: Option(Expr),
  )
}

pub type StructInnerMember {
  StructNamespace(List(ImplMember))
  StructMember(ImplMember)
  StructImpl(
    visibility: Option(Visibility),
    interface: Ident,
    generics: List(GenericArg),
    members: List(ImplMember),
  )
}

pub type ImplMember {
  ImplLet(meta: LetDeclaration, value: Expr)
  ImplFunc(meta: FuncDeclaration, body: Expr)
  ImplTypeAlias(meta: TypeAliasDeclaration, value: Type)
}

pub type InterfaceInnerBlock {
  InterfaceNamespace(List(ImplMember))
  InterfaceImpl(ImplExtensionDeclaration)
}

pub type InterfaceMember {
  InterfaceLet(meta: LetDeclaration, value: Expr)
  InterfaceFunc(meta: FuncDeclaration, body: Expr)
  InterfaceTypeAlias(
    meta: TypeAliasDeclaration,
    constraint: Option(Type),
    value: Option(Type),
  )
}

pub type EnumVariant {
  EnumVariant(
    visibility: Option(Visibility),
    name: Ident,
    fields: List(StructField),
  )
}

pub type FuncParam {
  FuncParam(spread: Bool, name: Pattern, type_: Type, default: Option(Expr))
}
