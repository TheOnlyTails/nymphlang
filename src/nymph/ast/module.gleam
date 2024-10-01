import nymph/ast/declaration

pub type Module {
  Module(path: String, members: List(declaration.Declaration))
}
