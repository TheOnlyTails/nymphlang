import chomp.{do, do_in, fail, optional, return, token}
import chomp/pratt
import gleam/dict
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/string
import nymph/ast/declaration
import nymph/ast/expr
import nymph/ast/operators
import nymph/ast/types
import nymph/parser/precedence
import nymph/parser/token

type Parser(a) =
  chomp.Parser(a, String, token.NymphToken, String)

pub fn parser() -> Parser(declaration.Module) {
  parse_module()
}

fn parse_module() {
  chomp.until_end(parse_declaration())
  |> chomp.map(declaration.Module)
}

fn parse_declaration() {
  chomp.one_of([
    parse_import(),
    {
      use visibility <- do(optional(parse_visibility()))
      chomp.one_of([
        parse_let(visibility)
          |> chomp.map(fn(it) { declaration.Let(it.0, it.1) }),
        parse_func(visibility)
          |> chomp.map(fn(it) { declaration.Func(it.0, it.1) }),
        parse_type_alias(visibility)
          |> chomp.map(fn(it) { declaration.TypeAlias(it.0, it.1) }),
        parse_struct(visibility),
        parse_enum(visibility),
        parse_namespace(visibility),
        parse_interface(visibility),
        parse_impl_for(visibility) |> chomp.backtrackable,
        parse_impl_ext(visibility),
      ])
    },
  ])
}

fn parse_struct(visibility) {
  use _ <- do_in("struct declaration", try_token(token.Struct))
  use name <- do(parse_identifier())
  use generics <- do(chomp.or(parse_generic_params(), []))
  use #(fields, members) <- do(delimited(
    try_token(token.LBrace),
    {
      use fields <- do(sequence_trailing(
        parse_struct_field(),
        try_token(token.Comma),
      ))
      use members <- do(chomp.many(parse_struct_inner_member()))
      return(#(fields, members))
    },
    try_token(token.RBrace),
  ))

  return(declaration.Struct(visibility:, name:, generics:, fields:, members:))
}

fn parse_struct_field() {
  use visibility <- do_in("struct field", optional(parse_visibility()))
  use mutable <- do(optional(try_token(token.Mut)) |> chomp.map(option.is_some))
  use name <- do(parse_identifier())
  use _ <- do(try_token(token.Colon))
  use type_ <- do(parse_type())
  use default <- do(
    optional({
      use _ <- do(try_token(token.Eq))
      use default <- do(parse_expr())
      return(default)
    }),
  )

  return(declaration.StructField(default:, mutable:, name:, type_:, visibility:))
}

fn parse_struct_inner_member() {
  chomp.one_of([
    parse_impl_member() |> chomp.map(declaration.StructMember),
    parse_struct_namespace() |> chomp.map(declaration.StructNamespace),
    parse_struct_impl()
      |> chomp.map(fn(it) {
        let #(interface, generics, members) = it
        declaration.StructImpl(generics:, interface:, members:)
      }),
  ])
}

fn parse_struct_namespace() {
  use _ <- do(try_token(token.Namespace))
  use members <- do(delimited(
    try_token(token.LBrace),
    chomp.many(parse_impl_member()),
    try_token(token.RBrace),
  ))
  return(members)
}

fn parse_struct_impl() {
  use _ <- do(try_token(token.Impl))
  use interface <- do(parse_identifier())
  use generics <- do(chomp.or(parse_generic_args(), []))
  use members <- do(delimited(
    try_token(token.LBrace),
    chomp.many(parse_impl_member()),
    try_token(token.RBrace),
  ))

  return(#(interface, generics, members))
}

fn parse_impl_ext(visibility) {
  use _ <- do(try_token(token.Impl))
  use generic_params <- do(chomp.or(parse_generic_params(), []))
  use name <- do(parse_identifier())
  use generic_args <- do(chomp.or(parse_generic_args(), []))
  use members <- do(delimited(
    try_token(token.LBrace),
    chomp.many(parse_impl_member()),
    try_token(token.RBrace),
  ))

  return(declaration.ImplExtension(
    visibility:,
    name:,
    generic_params:,
    generic_args:,
    members:,
  ))
}

fn parse_impl_for(visibility) {
  use _ <- do(try_token(token.Impl))
  use generics <- do(chomp.or(parse_generic_params(), []))
  use super_name <- do(parse_identifier())
  use super_generics <- do(chomp.or(parse_generic_args(), []))
  use _ <- do(try_token(token.For))
  use sub_name <- do(parse_identifier())
  use sub_generics <- do(chomp.or(parse_generic_args(), []))

  use members <- do(delimited(
    try_token(token.LBrace),
    chomp.many(parse_impl_member()),
    try_token(token.RBrace),
  ))

  return(declaration.ImplFor(
    visibility:,
    generics:,
    super_name:,
    super_generics:,
    sub_name:,
    sub_generics:,
    members:,
  ))
}

fn parse_impl_member() {
  use visibility <- do(optional(parse_visibility()))

  chomp.one_of([
    parse_let(visibility)
      |> chomp.map(fn(it) { declaration.ImplLet(it.0, it.1) }),
    parse_func(visibility)
      |> chomp.map(fn(it) { declaration.ImplFunc(it.0, it.1) }),
  ])
}

fn parse_enum(visibility) {
  use _ <- do_in("enum declaration", try_token(token.Enum))
  use name <- do(parse_identifier())
  use generics <- do(chomp.or(parse_generic_params(), []))
  use #(variants, members) <- do(delimited(
    try_token(token.LBrace),
    {
      use variants <- do(sequence_trailing1(
        parse_enum_variant(),
        try_token(token.Comma),
      ))
      use members <- do(chomp.many(parse_struct_inner_member()))
      return(#(variants, members))
    },
    try_token(token.RBrace),
  ))

  return(declaration.Enum(visibility:, name:, generics:, variants:, members:))
}

fn parse_enum_variant() {
  use name <- do(parse_identifier())
  use fields <- do(
    chomp.or(
      delimited(
        try_token(token.LBrace),
        sequence_trailing(parse_struct_field(), try_token(token.Comma)),
        try_token(token.RBrace),
      ),
      [],
    ),
  )

  return(declaration.EnumVariant(name:, fields:))
}

fn parse_namespace(visibility) {
  use _ <- do_in("namespace declaration", try_token(token.Namespace))
  use name <- do(parse_identifier())
  use members <- do(delimited(
    try_token(token.LBrace),
    chomp.many(parse_impl_member()),
    try_token(token.RBrace),
  ))

  return(declaration.Namespace(members:, name:, visibility:))
}

fn parse_interface(visibility) {
  use _ <- do_in("interface declaration", try_token(token.Interface))
  use name <- do(parse_identifier())
  use generics <- do(chomp.or(parse_generic_params(), []))
  use members <- do(delimited(
    try_token(token.LBrace),
    chomp.many(parse_interface_member()),
    try_token(token.RBrace),
  ))

  return(declaration.Interface(generics:, members:, name:, visibility:))
}

fn parse_interface_member() {
  chomp.one_of([
    parse_struct_namespace() |> chomp.map(declaration.InterfaceNamespace),
    parse_struct_impl()
      |> chomp.map(fn(it) {
        let #(interface, generics, members) = it
        declaration.InterfaceImpl(generics:, interface:, members:)
      }),
    parse_let_interface(),
    parse_func_interface(),
  ])
}

fn parse_let_interface() -> Parser(declaration.InterfaceMember) {
  use visibility <- do(optional(parse_visibility()))
  use _ <- do_in("let declaration", try_token(token.Let))
  use mutable <- do(optional(try_token(token.Mut)) |> chomp.map(option.is_some))
  use name <- do(parse_identifier())
  use type_ <- do(
    optional({
      use _ <- do(try_token(token.Colon))
      use type_ <- do(parse_type())
      return(type_)
    }),
  )
  use value <- do(
    optional({
      use _ <- do(try_token(token.Eq))
      parse_expr()
    }),
  )

  return(declaration.InterfaceLet(
    meta: declaration.LetDeclaration(visibility:, mutable:, name:, type_:),
    value:,
  ))
}

fn parse_let(visibility) {
  use _ <- do_in("let declaration", try_token(token.Let))
  use mutable <- do(optional(try_token(token.Mut)) |> chomp.map(option.is_some))
  use name <- do(parse_identifier())
  use type_ <- do(
    optional({
      use _ <- do(try_token(token.Colon))
      use type_ <- do(parse_type())
      return(type_)
    }),
  )
  use _ <- do(try_token(token.Eq))
  use value <- do(parse_expr())

  return(#(
    declaration.LetDeclaration(visibility:, mutable:, name:, type_:),
    value,
  ))
}

pub fn parse_func_interface() {
  use visibility <- do(optional(parse_visibility()))
  use _ <- do_in("function declaration", try_token(token.Func))
  use name <- do(parse_identifier())
  use generics <- do(chomp.or(parse_generic_params(), []))
  use params <- do(delimited(
    try_token(token.LParen),
    sequence_trailing(parse_func_param(), try_token(token.Comma)),
    try_token(token.RParen),
  ))
  use return_type <- do(
    optional({
      use _ <- do(try_token(token.Colon))
      use it <- do(parse_type())
      return(it)
    }),
  )
  use body <- do(
    optional({
      use _ <- do(try_token(token.Arrow))
      use body <- do(parse_expr())
      return(body)
    }),
  )

  return(declaration.InterfaceFunc(
    declaration.FuncDeclaration(
      visibility:,
      name:,
      generics:,
      params:,
      return_type:,
    ),
    body,
  ))
}

pub fn parse_func(visibility) {
  use _ <- do_in("function declaration", try_token(token.Func))
  use name <- do(parse_identifier())
  use generics <- do(chomp.or(parse_generic_params(), []))
  use params <- do(delimited(
    try_token(token.LParen),
    sequence_trailing(parse_func_param(), try_token(token.Comma)),
    try_token(token.RParen),
  ))
  use return_type <- do(
    optional({
      use _ <- do(try_token(token.Colon))
      use it <- do(parse_type())
      return(it)
    }),
  )
  use _ <- do(try_token(token.Arrow))
  use body <- do(parse_expr())

  return(#(
    declaration.FuncDeclaration(
      visibility:,
      name:,
      generics:,
      params:,
      return_type:,
    ),
    body,
  ))
}

fn parse_func_param() -> Parser(expr.FuncParam) {
  use spread <- do_in(
    "function parameter",
    optional(try_token(token.Spread)) |> chomp.map(option.is_some),
  )
  use name <- do(chomp.backtrackable(parse_pattern()))
  use _ <- do(try_token(token.Colon))
  use type_ <- do(parse_type())
  use default <- do(
    optional({
      use _ <- do(try_token(token.Eq))
      use it <- do(parse_expr())
      return(it)
    }),
  )

  return(expr.FuncParam(spread:, name:, type_:, default:))
}

fn parse_type_alias(visibility) {
  use _ <- do_in("type alias declaration", try_token(token.Type))
  use name <- do(parse_identifier())
  use generics <- do(chomp.or(parse_generic_params(), []))
  use _ <- do(try_token(token.Eq))
  use value <- do(parse_type())

  return(#(
    declaration.TypeAliasDeclaration(visibility:, name:, generics:),
    value,
  ))
}

fn parse_import() {
  let import_ident =
    chomp.lazy(fn() {
      use name <- do_in("import identifier", parse_identifier())
      use alias <- do(
        chomp.optional({
          use _ <- do(try_token(token.As))
          use val <- do(parse_identifier())
          return(val)
        }),
      )

      #(name, alias) |> return
    })
  let with_clause =
    chomp.lazy(fn() {
      use _ <- do(try_token(token.With))
      use idents <- do(delimited(
        try_token(token.LParen),
        sequence_trailing(import_ident, try_token(token.Comma)),
        try_token(token.RParen),
      ))

      return(dict.from_list(idents))
    })
  use _ <- do_in("import statement", try_token(token.Import))
  use path <- do(sequence_trailing(parse_identifier(), try_token(token.Slash)))
  use idents <- do(chomp.optional(with_clause))

  return(declaration.Import(path, idents))
}

pub fn parse_expr() {
  pratt.expression(
    one_of: [
      fn(_) { parse_block_expr() },
      fn(_) { parse_identifier() |> chomp.map(expr.Identifier) },
      fn(_) { parse_int() |> chomp.map(expr.Int) },
      fn(_) { parse_float() |> chomp.map(expr.Float) },
      fn(_) { parse_char() |> chomp.map(expr.Char) },
      fn(_) { parse_boolean() |> chomp.map(expr.Boolean) },
      fn(config) { parse_string(config) |> chomp.map(expr.String) },
      fn(_) { try_token(token.This) |> chomp.replace(expr.This) },
      fn(_) { try_token(token.Underscore) |> chomp.replace(expr.Placeholder) },
      fn(_) { parse_ex_range_full() },
      fn(_) { parse_continue() },
      parse_for_loop,
      parse_while_loop,
      parse_if,
      parse_match,
      parse_return,
      parse_break,
      // negate number
      pratt.prefix(precedence.unary_op, try_token(token.Minus), expr.PrefixOp(
        operators.Negate,
        _,
      )),
      // boolean NOT
      pratt.prefix(
        precedence.unary_op,
        try_token(token.ExclamationMark),
        expr.PrefixOp(operators.Not, _),
      ),
      parse_closure,
      fn(config) {
        delimited(
          try_token(token.LParen),
          pratt.sub_expression(config, 0),
          try_token(token.RParen),
        )
        |> chomp.in("grouped expression")
        |> chomp.map(expr.Grouped)
      },
    ],
    and_then: [
      // ranges
      parse_ex_range(),
      parse_in_range(),
      // foo(a, b), a.b, a[b]
      parse_function_call(),
      parse_member_access(),
      parse_index_access(),
      // unary operators
      pratt.postfix(
        precedence.unary_op,
        try_token(token.PlusPlus),
        expr.PostfixOp(operators.Increment, _),
      ),
      pratt.postfix(
        precedence.unary_op,
        try_token(token.MinusMinus),
        expr.PostfixOp(operators.Decrement, _),
      ),
      parse_type_op(),
      // exponentiation
      infix_op(
        pratt.Right(precedence.exponent),
        token.StarStar,
        operators.Power,
        expr.BinaryOp,
      ),
      // multiplication/division/modulus
      infix_op(
        pratt.Left(precedence.times_divide),
        token.Star,
        operators.Times,
        expr.BinaryOp,
      ),
      infix_op(
        pratt.Left(precedence.times_divide),
        token.Slash,
        operators.Divide,
        expr.BinaryOp,
      ),
      infix_op(
        pratt.Left(precedence.times_divide),
        token.Percent,
        operators.Modulus,
        expr.BinaryOp,
      ),
      // addition/subtraction
      infix_op(
        pratt.Left(precedence.plus_minus),
        token.Plus,
        operators.Plus,
        expr.BinaryOp,
      ),
      infix_op(
        pratt.Left(precedence.plus_minus),
        token.Minus,
        operators.Minus,
        expr.BinaryOp,
      ),
      // bit shift ops
      infix_op(
        pratt.Left(precedence.ordering),
        token.LtLt,
        operators.LeftShift,
        expr.BinaryOp,
      ),
      infix_op(
        pratt.Left(precedence.ordering),
        token.GtGt,
        operators.RightShift,
        expr.BinaryOp,
      ),
      // bitwise ops
      infix_op(
        pratt.Left(precedence.bitwise_and),
        token.And,
        operators.BitAnd,
        expr.BinaryOp,
      ),
      infix_op(
        pratt.Left(precedence.bitwise_xor),
        token.Caret,
        operators.BitXor,
        expr.BinaryOp,
      ),
      infix_op(
        pratt.Left(precedence.bitwise_or),
        token.Pipe,
        operators.BitOr,
        expr.BinaryOp,
      ),
      // comparison ops
      infix_op(
        pratt.Left(precedence.comparison),
        token.EqEq,
        operators.Equals,
        expr.BinaryOp,
      ),
      infix_op(
        pratt.Left(precedence.comparison),
        token.NotEq,
        operators.NotEquals,
        expr.BinaryOp,
      ),
      infix_op(
        pratt.Left(precedence.comparison),
        token.Lt,
        operators.LessThan,
        expr.BinaryOp,
      ),
      infix_op(
        pratt.Left(precedence.comparison),
        token.LtEq,
        operators.LessThanEquals,
        expr.BinaryOp,
      ),
      infix_op(
        pratt.Left(precedence.comparison),
        token.Gt,
        operators.GreaterThan,
        expr.BinaryOp,
      ),
      infix_op(
        pratt.Left(precedence.comparison),
        token.GtEq,
        operators.GreaterThanEquals,
        expr.BinaryOp,
      ),
      infix_op(
        pratt.Left(precedence.comparison),
        token.In,
        operators.In,
        expr.BinaryOp,
      ),
      infix_op(
        pratt.Left(precedence.comparison),
        token.NotIn,
        operators.NotIn,
        expr.BinaryOp,
      ),
      // boolean ops
      infix_op(
        pratt.Left(precedence.boolean_and),
        token.AndAnd,
        operators.BoolAnd,
        expr.BinaryOp,
      ),
      infix_op(
        pratt.Left(precedence.boolean_or),
        token.PipePipe,
        operators.BoolOr,
        expr.BinaryOp,
      ),
      // pipeline op
      infix_op(
        pratt.Left(precedence.pipe),
        token.Triangle,
        operators.Pipe,
        expr.BinaryOp,
      ),
      // assignment ops
      infix_op(
        pratt.Left(precedence.assignment),
        token.Eq,
        operators.Assign,
        expr.AssignOp,
      ),
      infix_op(
        pratt.Left(precedence.assignment),
        token.PlusEq,
        operators.PlusAssign,
        expr.AssignOp,
      ),
      infix_op(
        pratt.Left(precedence.assignment),
        token.MinusEq,
        operators.MinusAssign,
        expr.AssignOp,
      ),
      infix_op(
        pratt.Left(precedence.assignment),
        token.StarEq,
        operators.TimesAssign,
        expr.AssignOp,
      ),
      infix_op(
        pratt.Left(precedence.assignment),
        token.SlashEq,
        operators.DivideAssign,
        expr.AssignOp,
      ),
      infix_op(
        pratt.Left(precedence.assignment),
        token.PercentEq,
        operators.ModulusAssign,
        expr.AssignOp,
      ),
      infix_op(
        pratt.Left(precedence.assignment),
        token.StarStarEq,
        operators.PowerAssign,
        expr.AssignOp,
      ),
      infix_op(
        pratt.Left(precedence.assignment),
        token.AndAndEq,
        operators.BoolAndAssign,
        expr.AssignOp,
      ),
      infix_op(
        pratt.Left(precedence.assignment),
        token.PipePipeEq,
        operators.BoolOrAssign,
        expr.AssignOp,
      ),
      infix_op(
        pratt.Left(precedence.assignment),
        token.AndEq,
        operators.BitAndAssign,
        expr.AssignOp,
      ),
      infix_op(
        pratt.Left(precedence.assignment),
        token.PipeEq,
        operators.BitOrAssign,
        expr.AssignOp,
      ),
      infix_op(
        pratt.Left(precedence.assignment),
        token.CaretEq,
        operators.BitXorAssign,
        expr.AssignOp,
      ),
      infix_op(
        pratt.Left(precedence.assignment),
        token.LtLtEq,
        operators.LeftShiftAssign,
        expr.AssignOp,
      ),
      infix_op(
        pratt.Left(precedence.assignment),
        token.GtGtEq,
        operators.RightShiftAssign,
        expr.AssignOp,
      ),
    ],
    or_error: "Expected expression",
  )
}

pub fn parse_type() {
  pratt.expression(
    one_of: [
      fn(_) { try_token(token.IntType) |> chomp.replace(types.IntType) },
      fn(_) { try_token(token.FloatType) |> chomp.replace(types.FloatType) },
      fn(_) { try_token(token.CharType) |> chomp.replace(types.CharType) },
      fn(_) { try_token(token.StringType) |> chomp.replace(types.StringType) },
      fn(_) { try_token(token.BooleanType) |> chomp.replace(types.BooleanType) },
      fn(_) { try_token(token.VoidType) |> chomp.replace(types.VoidType) },
      fn(_) { try_token(token.NeverType) |> chomp.replace(types.NeverType) },
      fn(_) { try_token(token.Underscore) |> chomp.replace(types.InferType) },
      // list type
      fn(config) {
        delimited(
          try_token(token.ListStart),
          pratt.sub_expression(config, 0),
          try_token(token.RBracket),
        )
        |> chomp.in("list type")
        |> chomp.map(types.ListType)
      },
      // map type
      fn(config) {
        delimited(
          try_token(token.MapStart),
          {
            use key <- do_in("map type", pratt.sub_expression(config, 0))
            use _ <- do_in("map type", try_token(token.Colon))
            use value <- do_in("map type", pratt.sub_expression(config, 0))
            return(types.MapType(key, value))
          },
          try_token(token.RBrace),
        )
        |> chomp.in("map type")
      },
      // tuple type
      fn(config) {
        delimited(
          try_token(token.TupleStart),
          sequence_trailing(
            pratt.sub_expression(config, 0),
            try_token(token.Comma),
          ),
          try_token(token.RParen),
        )
        |> chomp.in("tuple type")
        |> chomp.map(types.TupleType)
      },
      // function type
      fn(config) {
        use params <- do_in(
          "function type",
          delimited(
            try_token(token.LParen),
            sequence_trailing(
              pratt.sub_expression(config, 0),
              try_token(token.Comma),
            ),
            try_token(token.RParen),
          ),
        )
        use _ <- do(try_token(token.Arrow))
        use return_type <- do(pratt.sub_expression(config, 1))

        return(types.FuncType(params:, return_type:))
      },
      // reference type
      fn(_) {
        use name <- do_in("reference type", parse_identifier())
        use generics <- do(chomp.or(parse_generic_args(), []))

        return(types.Reference(name:, generics:))
      },
      fn(config) {
        delimited(
          try_token(token.LParen),
          pratt.sub_expression(config, 0),
          try_token(token.RParen),
        )
        |> chomp.in("grouped type")
        |> chomp.map(types.GroupedType)
      },
    ],
    and_then: [
      pratt.infix(pratt.Left(1), try_token(token.Plus), types.Intersection),
    ],
    or_error: "Expected type",
  )
  |> chomp.backtrackable
}

fn parse_int() -> Parser(Int) {
  {
    use t <- chomp.take_map

    case t {
      token.BinaryInt(value)
      | token.OctalInt(value)
      | token.HexInt(value)
      | token.DecimalInt(value) -> Some(value)
      _ -> None
    }
  }
  |> chomp.in("integer literal")
}

fn parse_float() -> Parser(Float) {
  {
    use t <- chomp.take_map()

    case t {
      token.FloatBothParts(value)
      | token.FloatFirstPart(value)
      | token.FloatSecondPart(value)
      | token.FloatNoExponent(value)
      | token.FloatInt(value) -> Some(value)
      _ -> None
    }
  }
  |> chomp.in("float literal")
}

fn parse_boolean() -> Parser(Bool) {
  {
    use t <- chomp.take_map()

    case t {
      token.True -> Some(True)
      token.False -> Some(False)
      _ -> None
    }
  }
  |> chomp.in("boolean literal")
}

fn parse_char() -> Parser(UtfCodepoint) {
  {
    use t <- chomp.take_map()

    case t {
      token.CharUnicode(value) ->
        string.utf_codepoint(value) |> option.from_result
      token.CharEscape(expr.Backslash) ->
        string.utf_codepoint(92) |> option.from_result
      token.CharEscape(expr.Newline) ->
        string.utf_codepoint(10) |> option.from_result
      token.CharEscape(expr.Carriage) ->
        string.utf_codepoint(13) |> option.from_result
      token.CharEscape(expr.Tab) ->
        string.utf_codepoint(9) |> option.from_result
      token.CharEscape(expr.Apostrophe) ->
        string.utf_codepoint(39) |> option.from_result
      token.CharEscape(_) -> None
      token.Char(value) -> Some(value)
      _ -> None
    }
  }
  |> chomp.in("char literal")
}

fn parse_string(config) -> Parser(List(expr.StringPart)) {
  use _ <- do_in("string literal", try_token(token.StringStart))
  use value <- do(chomp.until(
    chomp.one_of([
      {
        use t <- do(chomp.any())

        case t {
          token.StringUnicode(codepoint) ->
            case string.utf_codepoint(codepoint) {
              Ok(val) -> return(expr.Unicode(val))
              Error(_) ->
                fail(
                  "Invalid unicode codepoint: U+" <> int.to_base16(codepoint),
                )
            }
          token.StringEscape(escape) -> return(expr.EscapeSequence(escape))
          token.StringChar(char) -> return(expr.Grapheme(char))
          token.StringInterpolationStart -> {
            use value <- do_in(
              "interpolated expression",
              pratt.sub_expression(config, 0),
            )
            use _ <- do(try_token(token.StringInterpolationEnd))
            return(expr.InterpolatedExpr(value))
          }
          _ ->
            panic as "Unexpected token in string literal, should be prevented in lexing"
        }
      },
    ]),
    token.StringEnd,
  ))

  return(value)
}

fn parse_ex_range_full() {
  try_token(token.DotDot)
  |> chomp.replace(expr.Range(expr.Full))
}

fn parse_ex_range() {
  pratt.infix(
    pratt.Left(precedence.range),
    try_token(token.DotDot),
    fn(min, max) { expr.Range(expr.Exclusive(min, max)) },
  )
}

fn parse_in_range() {
  pratt.infix(
    pratt.Left(precedence.range),
    try_token(token.DotDotEq),
    fn(min, max) { expr.Range(expr.Inclusive(min, max)) },
  )
}

fn parse_closure(config) -> Parser(expr.Expr) {
  config
  |> pratt.prefix_custom(
    precedence.closure,
    fn(_) {
      use generics <- do_in(
        "closure expression",
        chomp.or(parse_generic_params(), []),
      )
      use params <- do(
        delimited(
          try_token(token.LParen),
          sequence_trailing(parse_func_param(), try_token(token.Comma)),
          try_token(token.RParen),
        )
        |> chomp.backtrackable,
      )
      use return_type <- do(
        optional({
          use _ <- do(try_token(token.Colon))
          use it <- do(parse_type())
          return(it)
        }),
      )
      use _ <- do(try_token(token.Arrow))

      return(expr.Closure(params:, generics:, return_type:, body: _))
    },
    fn(body, data) { data(body) },
  )
}

fn parse_for_loop(config) -> Parser(expr.Expr) {
  config
  |> pratt.prefix_custom(
    0,
    fn(config) {
      use _ <- do_in("for loop", try_token(token.For))
      use label <- do_in(
        "for loop label",
        optional({
          use _ <- do(try_token(token.AtSign))
          use label <- do(parse_identifier())
          return(label)
        }),
      )
      use _ <- do(try_token(token.LParen))
      use variable <- do(parse_pattern())
      use _ <- do(try_token(token.In))
      use iterable <- do(pratt.sub_expression(config, 0))
      use _ <- do(try_token(token.RParen))

      return(expr.For(variable:, iterable:, label:, body: _))
    },
    fn(body, data) { data(body) },
  )
}

fn parse_while_loop(config) -> Parser(expr.Expr) {
  config
  |> pratt.prefix_custom(
    0,
    fn(config) {
      use _ <- do_in("while loop", try_token(token.While))
      use label <- do(
        optional({
          use _ <- do_in("while loop label", try_token(token.AtSign))
          use label <- do(parse_identifier())
          return(label)
        }),
      )
      use _ <- do(try_token(token.LParen))
      use condition <- do(pratt.sub_expression(config, 0))
      use _ <- do(try_token(token.RParen))
      return(expr.While(condition:, label:, body: _))
    },
    fn(body, data) { data(body) },
  )
}

fn parse_if(config) -> Parser(expr.Expr) {
  use _ <- do_in("if expression", try_token(token.If))
  use _ <- do(try_token(token.LParen))
  use condition <- do(delimited(
    try_token(token.LParen),
    pratt.sub_expression(config, 0),
    try_token(token.RParen),
  ))
  use _ <- do(try_token(token.RParen))
  use then <- do(pratt.sub_expression(config, 0))
  use otherwise <- do(
    optional({
      use _ <- do_in("else clause", try_token(token.Else))
      use otherwise <- do(pratt.sub_expression(config, 0))
      return(otherwise)
    }),
  )

  return(expr.If(condition:, then:, otherwise:))
}

fn parse_match(config) {
  use _ <- do_in("match expression", try_token(token.Match))
  use value <- do(delimited(
    try_token(token.LParen),
    pratt.sub_expression(config, 0),
    try_token(token.RParen),
  ))
  use arms <- do(delimited(
    try_token(token.LBrace),
    sequence_trailing1(parse_match_arm(config), try_token(token.Comma)),
    try_token(token.RBrace),
  ))

  return(expr.Match(value:, arms:))
}

fn parse_match_arm(config) {
  use pattern <- do_in("match arm", parse_pattern())
  use guard <- do(
    optional({
      use _ <- do_in("match arm guard", try_token(token.If))
      use condition <- do(pratt.sub_expression(config, 0))
      return(condition)
    }),
  )
  use _ <- do(try_token(token.Arrow))
  use body <- do(pratt.sub_expression(config, 0))

  return(expr.MatchArm(pattern:, guard:, body:))
}

fn parse_function_call() {
  pratt.postfix_custom(
    precedence.function_call,
    fn(config) {
      use generics <- do_in("function call", chomp.or(parse_generic_args(), []))
      use args <- do(delimited(
        try_token(token.LParen),
        sequence_trailing(parse_call_arg(config), try_token(token.Comma)),
        try_token(token.RParen),
      ))

      return(expr.Call(func: _, generics:, args:))
    },
    fn(func, data) { data(func) },
  )
}

fn parse_call_arg(config) -> Parser(expr.CallArg) {
  use name <- do_in(
    "function call argument",
    optional({
      use name <- do(chomp.backtrackable(parse_identifier()))
      use _ <- do(try_token(token.Eq))
      return(name)
    }),
  )
  use spread <- do(
    optional(try_token(token.Spread)) |> chomp.map(option.is_some),
  )
  use value <- do(pratt.sub_expression(config, 0))

  return(expr.CallArg(name:, spread:, value:))
}

fn parse_member_access() {
  pratt.postfix_custom(
    precedence.member_access,
    fn(_) {
      use _ <- do_in("member access expression", try_token(token.Dot))
      use member <- do(parse_identifier())
      return(member)
    },
    expr.MemberAccess,
  )
}

fn parse_index_access() {
  pratt.postfix_custom(
    precedence.index_access,
    fn(config) {
      delimited(
        try_token(token.LBracket),
        pratt.sub_expression(config, 0),
        try_token(token.RBracket),
      )
      |> chomp.in("index access expression")
    },
    expr.IndexAccess,
  )
}

fn parse_return(config) -> Parser(expr.Expr) {
  use _ <- do_in("return expression", try_token(token.Return))
  use label <- do(
    optional({
      use _ <- do_in("return label", try_token(token.AtSign))
      use label <- do(parse_identifier())
      return(label)
    }),
  )
  use value <- do(
    optional(pratt.sub_expression(config, precedence.return_break)),
  )
  return(expr.Return(value:, label:))
}

fn parse_break(config) -> Parser(expr.Expr) {
  use _ <- do_in("break expression", try_token(token.Break))
  use label <- do(
    optional({
      use _ <- do_in("break label", try_token(token.AtSign))
      use label <- do(parse_identifier())
      return(label)
    }),
  )
  use value <- do(
    optional(pratt.sub_expression(config, precedence.return_break)),
  )
  return(expr.Break(value:, label:))
}

fn parse_continue() -> Parser(expr.Expr) {
  use _ <- do_in("continue expression", try_token(token.Continue))
  use label <- do(
    optional({
      use _ <- do_in("continue label", try_token(token.AtSign))
      use label <- do(parse_identifier())
      return(label)
    }),
  )

  return(expr.Continue(label))
}

fn parse_type_op() {
  pratt.postfix_custom(
    precedence.type_op,
    fn(_) {
      use op <- do_in(
        "type operation expression",
        chomp.one_of([
          try_token(token.As) |> chomp.replace(operators.As),
          try_token(token.Is) |> chomp.replace(operators.Is),
          try_token(token.NotIs) |> chomp.replace(operators.NotIs),
        ]),
      )
      use rhs <- do(parse_type())
      return(#(op, rhs))
    },
    fn(lhs, data) {
      let #(op, rhs) = data
      expr.TypeOp(lhs:, op:, rhs:)
    },
  )
}

fn parse_block_expr() {
  use label <- do(
    optional({
      use label <- do(chomp.backtrackable(parse_identifier()))
      use _ <- do(try_token(token.AtSign))
      return(label)
    }),
  )

  delimited(
    try_token(token.LBrace),
    chomp.many(
      chomp.one_of([
        parse_expr() |> chomp.map(expr.Expr),
        parse_let(None)
          |> chomp.map(fn(decl) {
            let #(
              declaration.LetDeclaration(mutable:, name:, type_:, visibility: _),
              value,
            ) = decl
            expr.Let(name:, type_:, mutable:, value:)
          }),
      ]),
    ),
    try_token(token.RBrace),
  )
  |> chomp.in("block expression")
  |> chomp.map(fn(body) { expr.Block(body:, label:) })
}

fn parse_pattern() -> Parser(expr.Pattern) {
  pratt.expression(
    one_of: [
      fn(_) {
        try_token(token.Underscore) |> chomp.replace(expr.PlaceholderPattern)
      },
      fn(_) {
        try_token(token.Spread)
        |> chomp.replace(expr.RestPattern)
      },
      fn(config) {
        use name <- do_in("struct pattern", parse_identifier())
        use fields <- do(
          chomp.or(
            delimited(
              try_token(token.LBrace),
              sequence_trailing1(
                chomp.one_of([
                  try_token(token.Spread) |> chomp.replace(expr.RestField),
                  {
                    use name <- do(parse_identifier())
                    use value <- do(
                      optional({
                        use _ <- do(try_token(token.Eq))
                        pratt.sub_expression(config, 0)
                      }),
                    )
                    return(expr.NamedField(name:, value:))
                  },
                ]),
                try_token(token.Comma),
              ),
              try_token(token.RBrace),
            ),
            [],
          ),
        )
        return(expr.StructPattern(name:, fields:))
      },
      fn(_) {
        use negative <- do_in(
          "integer pattern",
          optional(try_token(token.Minus)) |> chomp.map(option.is_some),
        )
        use value <- do(parse_int())

        expr.IntPattern(case negative {
          True -> -value
          False -> value
        })
        |> return
      },
      fn(_) {
        use negative <- do_in(
          "float pattern",
          optional(try_token(token.Minus)) |> chomp.map(option.is_some),
        )
        use value <- do(parse_float())

        expr.FloatPattern(case negative {
          True -> float.negate(value)
          False -> value
        })
        |> return
      },
      fn(_) { parse_boolean() |> chomp.map(expr.BooleanPattern) },
      fn(_) { parse_char() |> chomp.map(expr.CharPattern) },
      fn(config) {
        delimited(
          try_token(token.ListStart),
          sequence_trailing(
            pratt.sub_expression(config, 0),
            try_token(token.Comma),
          ),
          try_token(token.RBracket),
        )
        |> chomp.in("list pattern")
        |> chomp.map(expr.ListPattern)
      },
      fn(config) {
        delimited(
          try_token(token.TupleStart),
          sequence_trailing(
            pratt.sub_expression(config, 0),
            try_token(token.Comma),
          ),
          try_token(token.RParen),
        )
        |> chomp.in("tuple pattern")
        |> chomp.map(expr.TuplePattern)
      },
      fn(config) {
        delimited(
          try_token(token.MapStart),
          sequence_trailing(
            {
              use key <- do_in(
                "map pattern entry",
                pratt.sub_expression(config, 0),
              )
              use _ <- do(try_token(token.Colon))
              use value <- do(pratt.sub_expression(config, 0))
              return(expr.MapPatternEntry(key:, value:))
            },
            try_token(token.Comma),
          ),
          try_token(token.RBrace),
        )
        |> chomp.in("map pattern")
        |> chomp.map(expr.MapPattern)
      },
      fn(_) {
        use _ <- do_in("type pattern", try_token(token.Is))
        use it <- do(parse_type())
        return(expr.TypePattern(it))
      },
      pratt.prefix(3, try_token(token.DotDotEq), fn(max) {
        expr.RangePattern(expr.InclusivePatternMax(max:))
      }),
      fn(config) {
        delimited(
          try_token(token.LParen),
          pratt.sub_expression(config, 0) |> chomp.map(expr.GroupedPattern),
          try_token(token.RParen),
        )
        |> chomp.in("grouped pattern")
      },
    ],
    and_then: [
      pratt.postfix(3, try_token(token.DotDot), fn(min) {
        expr.RangePattern(expr.ExclusivePatternMin(min:))
      }),
      pratt.infix(pratt.Left(2), try_token(token.DotDot), fn(min, max) {
        expr.RangePattern(expr.ExclusivePatternBoth(min:, max:))
      }),
      pratt.infix(pratt.Left(2), try_token(token.DotDotEq), fn(min, max) {
        expr.RangePattern(expr.InclusivePatternBoth(min, max))
      }),
      pratt.infix(pratt.Left(1), try_token(token.Pipe), expr.DisjunctionPattern),
    ],
    or_error: "Expected pattern",
  )
}

fn parse_generic_args() -> Parser(List(types.GenericArg)) {
  delimited(
    try_token(token.Lt),
    sequence_trailing1(
      {
        use name <- do_in(
          "generic argument",
          optional({
            use it <- do_in(
              "generic argument",
              chomp.backtrackable(parse_identifier()),
            )
            use _ <- do_in("generic argument", try_token(token.Eq))
            return(it)
          }),
        )
        use value <- do_in("generic argument", parse_type())

        return(types.GenericArg(value:, name:))
      },
      try_token(token.Comma),
    ),
    try_token(token.Gt),
  )
  |> chomp.in("generic argument list")
}

fn parse_generic_params() -> Parser(List(types.GenericParam)) {
  delimited(
    try_token(token.Lt),
    sequence_trailing1(
      {
        use name <- do(parse_identifier())
        use constraint <- do(
          optional({
            use _ <- do(try_token(token.Colon))
            use it <- do(parse_type())
            return(it)
          }),
        )
        use default <- do(
          optional({
            use _ <- do(try_token(token.Eq))
            use it <- do(parse_type())
            return(it)
          }),
        )
        return(types.GenericParam(name:, constraint:, default:))
      },
      try_token(token.Comma),
    ),
    try_token(token.Gt),
  )
  |> chomp.in("generic parameter list")
}

fn parse_visibility() -> Parser(declaration.Visibility) {
  chomp.one_of([
    try_token(token.Public) |> chomp.replace(declaration.Public),
    try_token(token.Internal) |> chomp.replace(declaration.Internal),
    try_token(token.Private) |> chomp.replace(declaration.Private),
  ])
  |> chomp.backtrackable
}

fn parse_identifier() -> Parser(String) {
  {
    use t <- chomp.take_map

    case t {
      token.Identifier(name) -> Some(name)
      _ -> None
    }
  }
  |> chomp.in("identifier")
}

fn infix_op(
  precedence: pratt.Precedence,
  token: token.NymphToken,
  op: b,
  constructor: fn(a, b, a) -> a,
) {
  pratt.infix(precedence, try_token(token), fn(lhs, rhs) {
    constructor(lhs, op, rhs)
  })
}

fn try_token(tok: token.NymphToken) -> Parser(Nil) {
  use t <- chomp.take_map
  case t == tok {
    True -> Some(Nil)
    False -> None
  }
}

fn delimited(start: Parser(_), parser: Parser(a), end: Parser(_)) -> Parser(a) {
  use _ <- do(start)
  use it <- do(parser)
  use _ <- do(end)
  return(it)
}

fn sequence_trailing(
  parser: Parser(a),
  separator sep: Parser(x),
) -> Parser(List(a)) {
  use xs <- chomp.loop([])

  let end_of_sequence = fn(xs) {
    use <- chomp.lazy
    return(chomp.Break(list.reverse(xs)))
  }

  let continue = {
    use x <- do(parser)
    chomp.one_of([
      sep |> chomp.replace(chomp.Continue([x, ..xs])),
      end_of_sequence([x, ..xs]),
    ])
  }

  chomp.one_of([continue, end_of_sequence(xs)])
}

fn sequence_trailing1(
  parser: Parser(a),
  separator sep: Parser(x),
) -> Parser(List(a)) {
  use x <- do(parser)
  use xs <- do(
    {
      use _ <- do(sep)
      sequence_trailing1(parser, sep)
    }
    |> chomp.or([]),
  )
  return([x, ..xs])
}
