// import filepath
// import gleam/dict
// import gleam/list
// import gleam/option.{type Option}
// import nymph/ast/declaration
// import nymph/ast/types

// pub type Value {
//   IntValue(Int)
//   FloatValue(Float)
//   CharValue(UtfCodepoint)
//   StringValue(String)
//   BooleanValue(Bool)
//   ListValue(List(Value))
//   MapValue(dict.Dict(Value, Value))
//   TupleValue(List(Value))
// }

// pub type EnvItem {
//   Variable(mutable: Bool, type_: types.Type, value: Option(Value))
//   Function
// }

// type Environment =
//   List(#(String, EnvItem))

// pub type Context {
//   Context(package_root: String, modules: List(declaration.Module))
// }

// pub type Error {
//   UnresolvedModule(path: String)
//   SelfImport(path: String)
//   UnknownModuleMember
// }

// pub fn check_module(context: Context, module: declaration.Module) -> Result(Nil, List(Error)) {
//   module.members
//   |> list.map_fold(#([]), fn(environment: Environment, member) {
//     case member {
//       declaration.Import(path:, idents:) -> {
//         let current_path = module.path |> filepath.expand
//         let import_path = filepath.join(context.package_root, path |> string.join("/")) |> filepath.expand
//         case import_path {
//           current_path -> 
//         }
//       }
//     }
//   })
// }
