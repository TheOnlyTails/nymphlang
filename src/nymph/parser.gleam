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
    parse_let(),
    parse_func(),
    // parse_type_alias(),
  // parse_struct(),
  // parse_enum(),
  // parse_namespace(),
  // parse_interface(),
  // parse_impl_ext(),
  // parse_impl_for(),
  ])
}

fn parse_let() {
  use visibility <- do_in("let declaration", optional(parse_visibility()))
  use _ <- do(token(token.Let))
  use mutable <- do(optional(token(token.Mut)) |> chomp.map(option.is_some))
  use name <- do(parse_identifier())
  use type_ <- do(
    optional({
      use _ <- do(token(token.Colon))
      use type_ <- do(parse_type())
      return(type_)
    }),
  )
  use _ <- do(token(token.Eq))
  use value <- do(parse_expr())

  return(declaration.Let(
    declaration.LetDeclaration(visibility:, mutable:, name:, type_:),
    value,
  ))
}

pub fn parse_func() {
  use visibility <- do_in("function declaration", optional(parse_visibility()))
  use _ <- do(try_token(token.Func))
  use name <- do(parse_identifier())
  use generics <- do(chomp.or(parse_generic_params(), []))
  use params <- do(delimited(
    try_token(token.LParen),
    sequence_trailing(parse_func_param(), token(token.Comma)),
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

  return(declaration.Func(
    meta: declaration.FuncDeclaration(
      visibility:,
      name:,
      generics:,
      params:,
      return_type:,
    ),
    body:,
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

fn parse_import() {
  let import_ident =
    chomp.lazy(fn() {
      use name <- do_in("import identifier", parse_identifier())
      use alias <- do(
        chomp.optional({
          use _ <- do(token(token.As))
          use val <- do(parse_identifier())
          return(val)
        }),
      )

      #(name, alias) |> return
    })
  let with_clause =
    chomp.lazy(fn() {
      use _ <- do(token(token.With))
      use idents <- do(delimited(
        token(token.LParen),
        sequence_trailing(import_ident, token(token.Comma)),
        token(token.RParen),
      ))

      return(dict.from_list(idents))
    })
  use _ <- do_in("import statement", token(token.Import))
  use path <- do(sequence_trailing(parse_identifier(), token(token.Slash)))
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
      parse_closure,
      parse_for_loop,
      parse_while_loop,
      parse_if,
      parse_return,
      parse_break,
      // negate number
      pratt.prefix(precedence.unary_op, token(token.Minus), expr.PrefixOp(
        operators.Negate,
        _,
      )),
      // boolean NOT
      pratt.prefix(
        precedence.unary_op,
        token(token.ExclamationMark),
        expr.PrefixOp(operators.Not, _),
      ),
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
      pratt.postfix(precedence.unary_op, token(token.PlusPlus), expr.PostfixOp(
        operators.Increment,
        _,
      )),
      pratt.postfix(precedence.unary_op, token(token.MinusMinus), expr.PostfixOp(
        operators.Decrement,
        _,
      )),
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
          token(token.ListStart),
          pratt.sub_expression(config, 0),
          token(token.RBracket),
        )
        |> chomp.in("list type")
        |> chomp.map(types.ListType)
      },
      // map type
      fn(config) {
        delimited(
          token(token.MapStart),
          {
            use key <- do_in("map type", pratt.sub_expression(config, 0))
            use _ <- do_in("map type", token(token.Colon))
            use value <- do_in("map type", pratt.sub_expression(config, 0))
            return(types.MapType(key, value))
          },
          token(token.RBrace),
        )
        |> chomp.in("map type")
      },
      // tuple type
      fn(config) {
        delimited(
          token(token.TupleStart),
          sequence_trailing(pratt.sub_expression(config, 0), token(token.Comma)),
          token(token.RParen),
        )
        |> chomp.in("tuple type")
        |> chomp.map(types.TupleType)
      },
      // function type
      fn(config) {
        use params <- do_in(
          "function type",
          delimited(
            token(token.LParen),
            sequence_trailing(
              pratt.sub_expression(config, 0),
              token(token.Comma),
            ),
            token(token.RParen),
          ),
        )
        use _ <- do(token(token.Arrow))
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
          token(token.LParen),
          pratt.sub_expression(config, 0),
          token(token.RParen),
        )
        |> chomp.in("grouped type")
        |> chomp.map(types.GroupedType)
      },
    ],
    and_then: [
      pratt.infix(pratt.Left(1), token(token.Plus), types.Intersection),
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
            use _ <- do(token(token.StringInterpolationEnd))
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
  pratt.infix(pratt.Left(precedence.range), token(token.DotDot), fn(min, max) {
    expr.Range(expr.Exclusive(min, max))
  })
}

fn parse_in_range() {
  pratt.infix(pratt.Left(precedence.range), token(token.DotDotEq), fn(min, max) {
    expr.Range(expr.Inclusive(min, max))
  })
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
          use _ <- do(token(token.AtSign))
          use label <- do(parse_identifier())
          return(label)
        }),
      )
      use _ <- do(token(token.LParen))
      use variable <- do(parse_pattern())
      use _ <- do(token(token.In))
      use iterable <- do(pratt.sub_expression(config, 0))
      use _ <- do(token(token.RParen))

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
          use _ <- do_in("while loop label", token(token.AtSign))
          use label <- do(parse_identifier())
          return(label)
        }),
      )
      use _ <- do(token(token.LParen))
      use condition <- do(pratt.sub_expression(config, 0))
      use _ <- do(token(token.RParen))
      return(expr.While(condition:, label:, body: _))
    },
    fn(body, data) { data(body) },
  )
}

fn parse_if(config) -> Parser(expr.Expr) {
  use _ <- do_in("if expression", try_token(token.If))
  use _ <- do(token(token.LParen))
  use condition <- do(delimited(
    token(token.LParen),
    pratt.sub_expression(config, 0),
    token(token.RParen),
  ))
  use _ <- do(token(token.RParen))
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

fn parse_function_call() {
  pratt.postfix_custom(
    precedence.function_call,
    fn(config) {
      use generics <- do_in("function call", chomp.or(parse_generic_args(), []))
      use args <- do(delimited(
        token(token.LParen),
        sequence_trailing(parse_call_arg(config), token(token.Comma)),
        token(token.RParen),
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
      use _ <- do(token(token.Eq))
      return(name)
    }),
  )
  use spread <- do(optional(token(token.Spread)) |> chomp.map(option.is_some))
  use value <- do(pratt.sub_expression(config, 0))

  return(expr.CallArg(name:, spread:, value:))
}

fn parse_member_access() {
  pratt.postfix_custom(
    precedence.member_access,
    fn(_) {
      use _ <- do_in("member access expression", token(token.Dot))
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
        token(token.LBracket),
        pratt.sub_expression(config, 0),
        token(token.RBracket),
      )
      |> chomp.in("index access expression")
    },
    expr.IndexAccess,
  )
}

fn parse_return(config) -> Parser(expr.Expr) {
  use _ <- do_in("return expression", token(token.Return))
  use label <- do(
    optional({
      use _ <- do_in("return label", token(token.AtSign))
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
  use _ <- do_in("break expression", token(token.Break))
  use label <- do(
    optional({
      use _ <- do_in("break label", token(token.AtSign))
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
  use _ <- do_in("continue expression", token(token.Continue))
  use label <- do(
    optional({
      use _ <- do_in("continue label", token(token.AtSign))
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
          token(token.As) |> chomp.replace(operators.As),
          token(token.Is) |> chomp.replace(operators.Is),
          token(token.NotIs) |> chomp.replace(operators.NotIs),
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
      use _ <- do(token(token.AtSign))
      return(label)
    }),
  )

  delimited(
    token(token.LBrace),
    chomp.many(
      chomp.one_of([
        parse_expr() |> chomp.map(expr.Expr),
        parse_let()
          |> chomp.map(fn(decl) {
            let assert declaration.Let(
              value:,
              meta: declaration.LetDeclaration(
                mutable:,
                name:,
                type_:,
                visibility: _,
              ),
            ) = decl
            expr.Let(name:, type_:, mutable:, value:)
          }),
      ]),
    ),
    token(token.RBrace),
  )
  |> chomp.in("block expression")
  |> chomp.map(fn(body) { expr.Block(body:, label:) })
}

fn parse_pattern() -> Parser(expr.Pattern) {
  pratt.expression(
    one_of: [
      fn(_) {
        token(token.Underscore) |> chomp.replace(expr.PlaceholderPattern)
      },
      fn(_) { token(token.Spread) |> chomp.replace(expr.RestPattern) },
      fn(_) {
        use negative <- do_in(
          "integer pattern",
          optional(token(token.Minus)) |> chomp.map(option.is_some),
        )
        use value <- do_in("integer pattern", parse_int())

        expr.IntPattern(case negative {
          True -> -value
          False -> value
        })
        |> return
      },
      fn(_) {
        use negative <- do_in(
          "float pattern",
          optional(token(token.Minus)) |> chomp.map(option.is_some),
        )
        use value <- do_in("float pattern", parse_float())

        expr.FloatPattern(case negative {
          True -> float.negate(value)
          False -> value
        })
        |> return
      },
      fn(_) { parse_boolean() |> chomp.map(expr.BooleanPattern) },
      fn(_) { parse_char() |> chomp.map(expr.CharPattern) },
      fn(_) { parse_identifier() |> chomp.map(expr.IdentifierPattern) },
      fn(config) {
        delimited(
          token(token.ListStart),
          sequence_trailing(pratt.sub_expression(config, 0), token(token.Comma)),
          token(token.RBracket),
        )
        |> chomp.in("list pattern")
        |> chomp.map(expr.ListPattern)
      },
      fn(config) {
        delimited(
          token(token.TupleStart),
          sequence_trailing(pratt.sub_expression(config, 0), token(token.Comma)),
          token(token.RParen),
        )
        |> chomp.in("tuple pattern")
        |> chomp.map(expr.TuplePattern)
      },
      fn(config) {
        delimited(
          token(token.MapStart),
          sequence_trailing(
            chomp.one_of([
              token(token.Spread) |> chomp.replace(expr.RestEntry),
              {
                use key <- do_in(
                  "map pattern entry",
                  pratt.sub_expression(config, 0),
                )
                use _ <- do_in("map pattern entry", token(token.Colon))
                use value <- do_in(
                  "map pattern entry",
                  pratt.sub_expression(config, 0),
                )
                return(expr.Entry(key:, value:))
              },
            ]),
            token(token.Comma),
          ),
          token(token.RBrace),
        )
        |> chomp.in("map pattern")
        |> chomp.map(expr.MapPattern)
      },
      fn(_) {
        use _ <- do_in("type pattern", token(token.Is))
        use it <- do_in("type pattern", parse_type())
        return(expr.TypePattern(it))
      },
      fn(config) {
        use min <- do_in(
          "exclusive range pattern",
          pratt.sub_expression(config, 0),
        )
        use _ <- do_in("exclusive range pattern", token(token.DotDot))
        use max <- do_in(
          "exclusive range pattern",
          optional(pratt.sub_expression(config, 0)),
        )
        return(expr.RangePattern(expr.ExclusivePattern(min:, max:)))
      },
      fn(config) {
        use min <- do_in(
          "inclusive range pattern",
          optional(pratt.sub_expression(config, 0)),
        )
        use _ <- do_in("inclusive range pattern", token(token.DotDot))
        use max <- do_in(
          "inclusive range pattern",
          pratt.sub_expression(config, 0),
        )
        return(expr.RangePattern(expr.InclusivePattern(min:, max:)))
      },
      fn(config) {
        delimited(
          token(token.LParen),
          pratt.sub_expression(config, 0) |> chomp.map(expr.GroupedPattern),
          token(token.RParen),
        )
        |> chomp.in("grouped pattern")
      },
    ],
    and_then: [
      pratt.infix(pratt.Left(1), token(token.Pipe), expr.DisjunctionPattern),
    ],
    or_error: "Expected pattern",
  )
}

fn parse_generic_args() -> Parser(List(types.GenericArg)) {
  delimited(
    token(token.Lt),
    sequence_trailing1(
      {
        use name <- do_in(
          "generic argument",
          optional({
            use it <- do_in(
              "generic argument",
              chomp.backtrackable(parse_identifier()),
            )
            use _ <- do_in("generic argument", token(token.Eq))
            return(it)
          }),
        )
        use value <- do_in("generic argument", parse_type())

        return(types.GenericArg(value:, name:))
      },
      token(token.Comma),
    ),
    token(token.Gt),
  )
  |> chomp.in("generic argument list")
}

fn parse_generic_params() -> Parser(List(types.GenericParam)) {
  delimited(
    token(token.Lt),
    sequence_trailing1(
      {
        use name <- do(parse_identifier())
        use constraint <- do(
          optional({
            use _ <- do(token(token.Colon))
            use it <- do(parse_type())
            return(it)
          }),
        )
        use default <- do(
          optional({
            use _ <- do(token(token.Eq))
            use it <- do(parse_type())
            return(it)
          }),
        )
        return(types.GenericParam(name:, constraint:, default:))
      },
      token(token.Comma),
    ),
    token(token.Gt),
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
  pratt.infix(precedence, chomp.token(token), fn(lhs, rhs) {
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
  use items <- do(chomp.sequence(parser, sep))

  case items {
    [] -> return([])
    items -> {
      // optional trailing separator
      use _ <- do(chomp.optional(sep))
      return(items)
    }
  }
}

fn sequence_trailing1(
  parser: Parser(a),
  separator sep: Parser(x),
) -> Parser(List(a)) {
  use items <- do(chomp.sequence(parser, sep))
  use tail <- do(parser)
  use _ <- do(chomp.optional(sep))

  return(items |> list.append([tail]))
}
