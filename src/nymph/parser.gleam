import chomp.{do, do_in, fail, optional, return, token}
import chomp/pratt
import gleam/dict
import gleam/float
import gleam/int
import gleam/option
import gleam/string
import nymph/ast/declaration
import nymph/ast/expr
import nymph/ast/operators
import nymph/ast/types
import nymph/token

type Parser(a) =
  chomp.Parser(a, String, token.NymphToken, String)

pub fn parser() -> Parser(declaration.Module) {
  parse_module()
}

fn parse_module() {
  use members <- do(chomp.many(parse_declaration()))
  use _ <- do(chomp.end())
  // parses until the end of the input
  return(declaration.Module(members))
}

fn parse_declaration() {
  chomp.backtrackable(
    chomp.one_of([
      parse_import(),
      parse_let(),
      // parse_func(),
    // parse_type_alias(),
    // parse_struct(),
    // parse_enum(),
    // parse_namespace(),
    // parse_interface(),
    // parse_impl_ext(),
    // parse_impl_for(),
    ]),
  )
}

fn parse_let() {
  use visibility <- do_in("let declaration", optional(parse_visibility()))
  use _ <- do_in("let declaration", token(token.Let))
  use mutable <- do_in(
    "let declaration",
    optional(token(token.Mut)) |> chomp.map(option.is_some),
  )
  use name <- do_in("let declaration", parse_identifier())
  use type_ <- do_in(
    "let declaration",
    optional({
      use _ <- do_in("let declaration", token(token.Colon))
      use type_ <- do_in("let declaration", parse_type())
      return(type_)
    }),
  )
  use _ <- do_in("let declaration", token(token.Eq))
  use value <- do_in("let declaration", parse_expr())

  return(declaration.Let(
    declaration.LetDeclaration(visibility:, mutable:, name:, type_:),
    value,
  ))
}

fn parse_import() {
  let import_ident =
    chomp.lazy(fn() {
      use name <- do_in("import identifier", parse_identifier())
      use alias <- do_in(
        "import identifier",
        chomp.optional({
          use _ <- do_in("import identifier", token(token.As))
          use val <- do_in("import identifier", parse_identifier())
          return(val)
        }),
      )

      #(name, alias) |> return
    })
  let with_clause =
    chomp.lazy(fn() {
      use _ <- do_in("import statement", token(token.With))
      use _ <- do_in("import statement", token(token.LParen))
      use idents <- do_in(
        "import statement",
        sequence_trailing(import_ident, token(token.Comma)),
      )
      use _ <- do_in("import statement", token(token.RParen))

      dict.from_list(idents) |> return
    })
  use _ <- do_in("import statement", token(token.Import))
  use path <- do_in(
    "import statement",
    sequence_trailing(parse_identifier(), token(token.Slash)),
  )
  use idents <- do_in("import statement", chomp.optional(with_clause))

  return(declaration.Import(path, idents))
}

pub fn parse_expr() {
  pratt.expression(
    one_of: [
      fn(_) { parse_identifier() |> chomp.map(expr.Identifier) },
      fn(_) { parse_int() |> chomp.map(expr.Int) },
      fn(_) { parse_float() |> chomp.map(expr.Float) },
      fn(_) { parse_char() |> chomp.map(expr.Char) },
      fn(_) { parse_boolean() |> chomp.map(expr.Boolean) },
      fn(config) { parse_string(config) |> chomp.map(expr.String) },
      parse_function_call,
      parse_ex_range,
      parse_in_range,
      fn(_) { token(token.This) |> chomp.replace(expr.This) },
      fn(_) { token(token.Underscore) |> chomp.replace(expr.Placeholder) },
      parse_for_loop,
      parse_while_loop,
      parse_if,
      parse_member_access,
      parse_index_access,
      parse_return,
      parse_break,
      parse_continue,
      // negate number
      pratt.prefix(12, token(token.Minus), expr.PrefixOp(operators.Negate, _)),
      // boolean NOT
      pratt.prefix(12, token(token.ExclamationMark), expr.PrefixOp(
        operators.Not,
        _,
      )),
      parse_type_op,
      fn(config) {
        delimited(
          "grouped expression",
          token(token.LParen),
          pratt.sub_expression(config, 0) |> chomp.map(expr.Grouped),
          token(token.RParen),
        )
      },
    ],
    and_then: [
      // increment/decrement
      pratt.postfix(14, token(token.PlusPlus), expr.PostfixOp(
        operators.Increment,
        _,
      )),
      pratt.postfix(14, token(token.MinusMinus), expr.PostfixOp(
        operators.Decrement,
        _,
      )),
      // exponentiation
      infix_op(12, token.StarStar, operators.Power, expr.BinaryOp, False),
      // multiplication/division/modulus
      infix_op(11, token.Star, operators.Times, expr.BinaryOp, True),
      infix_op(11, token.Slash, operators.Divide, expr.BinaryOp, True),
      infix_op(11, token.Percent, operators.Modulus, expr.BinaryOp, True),
      // addition/subtraction
      infix_op(10, token.Plus, operators.Plus, expr.BinaryOp, True),
      infix_op(10, token.Minus, operators.Minus, expr.BinaryOp, True),
      // bit shift ops
      infix_op(9, token.LtLt, operators.LeftShift, expr.BinaryOp, True),
      infix_op(9, token.GtGt, operators.RightShift, expr.BinaryOp, True),
      // bitwise ops
      infix_op(8, token.And, operators.BitAnd, expr.BinaryOp, True),
      infix_op(7, token.Caret, operators.BitXor, expr.BinaryOp, True),
      infix_op(6, token.Pipe, operators.BitOr, expr.BinaryOp, True),
      // comparison ops
      infix_op(5, token.EqEq, operators.Equals, expr.BinaryOp, True),
      infix_op(5, token.NotEq, operators.NotEquals, expr.BinaryOp, True),
      infix_op(5, token.Lt, operators.LessThan, expr.BinaryOp, True),
      infix_op(5, token.LtEq, operators.LessThanEquals, expr.BinaryOp, True),
      infix_op(5, token.Gt, operators.GreaterThan, expr.BinaryOp, True),
      infix_op(5, token.GtEq, operators.GreaterThanEquals, expr.BinaryOp, True),
      infix_op(5, token.In, operators.In, expr.BinaryOp, True),
      infix_op(5, token.NotIn, operators.NotIn, expr.BinaryOp, True),
      // boolean ops
      infix_op(4, token.AndAnd, operators.BoolAnd, expr.BinaryOp, True),
      infix_op(3, token.PipePipe, operators.BoolOr, expr.BinaryOp, True),
      // pipeline op
      infix_op(2, token.Triangle, operators.Pipe, expr.BinaryOp, True),
      // assignment ops
      infix_op(1, token.Eq, operators.Assign, expr.AssignOp, True),
      infix_op(1, token.PlusEq, operators.PlusAssign, expr.AssignOp, True),
      infix_op(1, token.MinusEq, operators.MinusAssign, expr.AssignOp, True),
      infix_op(1, token.StarEq, operators.TimesAssign, expr.AssignOp, True),
      infix_op(1, token.SlashEq, operators.DivideAssign, expr.AssignOp, True),
      infix_op(1, token.PercentEq, operators.ModulusAssign, expr.AssignOp, True),
      infix_op(1, token.StarStarEq, operators.PowerAssign, expr.AssignOp, True),
      infix_op(1, token.AndAndEq, operators.BoolAndAssign, expr.AssignOp, True),
      infix_op(1, token.PipePipeEq, operators.BoolOrAssign, expr.AssignOp, True),
      infix_op(1, token.AndEq, operators.BitAndAssign, expr.AssignOp, True),
      infix_op(1, token.PipeEq, operators.BitOrAssign, expr.AssignOp, True),
      infix_op(1, token.CaretEq, operators.BitXorAssign, expr.AssignOp, True),
      infix_op(1, token.LtLtEq, operators.LeftShiftAssign, expr.AssignOp, True),
      infix_op(1, token.GtGtEq, operators.RightShiftAssign, expr.AssignOp, True),
    ],
    or_error: "Expected expression",
  )
}

pub fn parse_type() {
  pratt.expression(
    one_of: [
      fn(_) { token(token.IntType) |> chomp.replace(types.IntType) },
      fn(_) { token(token.FloatType) |> chomp.replace(types.FloatType) },
      fn(_) { token(token.CharType) |> chomp.replace(types.CharType) },
      fn(_) { token(token.StringType) |> chomp.replace(types.StringType) },
      fn(_) { token(token.BooleanType) |> chomp.replace(types.BooleanType) },
      fn(_) { token(token.VoidType) |> chomp.replace(types.VoidType) },
      fn(_) { token(token.NeverType) |> chomp.replace(types.NeverType) },
      fn(_) { token(token.Underscore) |> chomp.replace(types.InferType) },
      // list type
      fn(config) {
        delimited(
          "list type",
          token(token.ListStart),
          pratt.sub_expression(config, 0),
          token(token.RBracket),
        )
        |> chomp.map(types.ListType)
      },
      // map type
      fn(config) {
        delimited(
          "map type",
          token(token.MapStart),
          {
            use key <- do_in("map type", pratt.sub_expression(config, 0))
            use _ <- do_in("map type", token(token.Colon))
            use value <- do_in("map type", pratt.sub_expression(config, 0))
            return(types.MapType(key, value))
          },
          token(token.RBrace),
        )
      },
      // tuple type
      fn(config) {
        delimited(
          "tuple type",
          token(token.TupleStart),
          sequence_trailing(pratt.sub_expression(config, 0), token(token.Comma)),
          token(token.RParen),
        )
        |> chomp.map(types.TupleType)
      },
      // function type
      fn(config) {
        use params <- do_in(
          "function type",
          delimited(
            "function type",
            token(token.LParen),
            sequence_trailing(
              pratt.sub_expression(config, 0),
              token(token.Comma),
            ),
            token(token.RParen),
          ),
        )
        use _ <- do_in("function type", token(token.Arrow))
        use return_type <- do_in(
          "function type",
          pratt.sub_expression(config, 1),
        )

        return(types.FuncType(params:, return_type:))
      },
      // reference type
      fn(_) {
        use name <- do_in("reference type", parse_identifier())
        use generics <- do_in(
          "reference type",
          chomp.or(parse_generic_args(), []),
        )

        return(types.Reference(name:, generics:))
      },
      fn(config) {
        delimited(
          "grouped type",
          token(token.LParen),
          pratt.sub_expression(config, 0) |> chomp.map(types.GroupedType),
          token(token.RParen),
        )
      },
    ],
    and_then: [pratt.infix_left(1, token(token.Plus), types.Intersection)],
    or_error: "Expected type",
  )
  |> chomp.backtrackable
}

fn parse_int() -> Parser(Int) {
  use t <- do_in("integer literal", chomp.any())

  case t {
    token.BinaryInt(value)
    | token.OctalInt(value)
    | token.HexInt(value)
    | token.DecimalInt(value) -> return(value)
    _ -> fail("Expected an integer literal")
  }
}

fn parse_float() -> Parser(Float) {
  use t <- do_in("float literal", chomp.any())

  case t {
    token.FloatBothParts(value)
    | token.FloatFirstPart(value)
    | token.FloatSecondPart(value)
    | token.FloatNoExponent(value)
    | token.FloatInt(value) -> return(value)
    _ -> fail("Expected a float literal")
  }
}

fn parse_boolean() -> Parser(Bool) {
  use t <- do_in("boolean literal", chomp.any())

  case t {
    token.True -> return(True)
    token.False -> return(False)
    _ -> fail("Expected `true` or `false`")
  }
}

fn parse_char() -> Parser(UtfCodepoint) {
  use t <- do_in("char literal", chomp.any())

  case t {
    token.CharUnicode(value) ->
      case string.utf_codepoint(value) {
        Ok(val) -> return(val)
        Error(_) ->
          fail("Invalid unicode codepoint: U+" <> int.to_base16(value))
      }
    token.CharEscape(escape) ->
      case escape {
        expr.Backslash -> {
          let assert Ok(val) = string.utf_codepoint(92)
          return(val)
        }
        expr.Newline -> {
          let assert Ok(val) = string.utf_codepoint(10)
          return(val)
        }
        expr.Carriage -> {
          let assert Ok(val) = string.utf_codepoint(13)
          return(val)
        }
        expr.Tab -> {
          let assert Ok(val) = string.utf_codepoint(9)
          return(val)
        }
        expr.Apostrophe -> {
          let assert Ok(val) = string.utf_codepoint(39)
          return(val)
        }
        _ ->
          panic as "Invalid escape code in char literal, should be prevented in lexing"
      }
    token.Char(value) -> return(value)
    _ -> fail("Expected a char literal")
  }
}

fn parse_string(config) -> Parser(List(expr.StringPart)) {
  chomp.many(
    chomp.one_of([
      {
        use t <- do_in("string literal", chomp.any())

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
            use _ <- do_in(
              "interpolated_expr",
              token(token.StringInterpolationEnd),
            )
            return(expr.InterpolatedExpr(value))
          }
          _ ->
            panic as "Unexpected token in string literal, should be prevented in lexing"
        }
      },
    ]),
  )
  |> delimited(
    "string literal",
    token(token.StringStart),
    _,
    token(token.StringEnd),
  )
}

fn parse_ex_range(config) -> Parser(expr.Expr) {
  use min <- do(optional(pratt.sub_expression(config, 0)))
  use _ <- do(token(token.DotDot))
  use max <- do(optional(pratt.sub_expression(config, 0)))

  return(expr.Range(expr.Exclusive(min:, max:)))
}

fn parse_in_range(config) -> Parser(expr.Expr) {
  use min <- do(optional(pratt.sub_expression(config, 0)))
  use _ <- do(token(token.DotDot))
  use max <- do(pratt.sub_expression(config, 0))

  return(expr.Range(expr.Inclusive(min:, max:)))
}

fn parse_for_loop(config) -> Parser(expr.Expr) {
  use _ <- do_in("for loop", token(token.For))
  use label <- do_in(
    "for loop",
    optional({
      use _ <- do_in("for loop label", token(token.AtSign))
      use label <- do_in("for loop label", parse_identifier())
      return(label)
    }),
  )
  use _ <- do_in("for loop", token(token.LParen))
  use variable <- do_in("for loop", parse_pattern())
  use _ <- do_in("for loop", token(token.In))
  use iterable <- do_in("for loop", pratt.sub_expression(config, 0))
  use _ <- do_in("for loop", token(token.RParen))
  use body <- do_in("for loop", pratt.sub_expression(config, 0))

  return(expr.For(variable:, iterable:, label:, body:))
}

fn parse_while_loop(config) -> Parser(expr.Expr) {
  use _ <- do_in("while loop", token(token.While))
  use label <- do_in(
    "while loop",
    optional({
      use _ <- do_in("while loop label", token(token.AtSign))
      use label <- do_in("while loop label", parse_identifier())
      return(label)
    }),
  )
  use _ <- do_in("while loop", token(token.LParen))
  use condition <- do_in("while loop", pratt.sub_expression(config, 0))
  use _ <- do_in("while loop", token(token.RParen))
  use body <- do_in("while loop", pratt.sub_expression(config, 0))

  return(expr.While(condition:, label:, body:))
}

fn parse_if(config) -> Parser(expr.Expr) {
  use _ <- do_in("if expression", token(token.If))
  use _ <- do_in("if expression", token(token.LParen))
  use condition <- do_in("if expression", pratt.sub_expression(config, 0))
  use _ <- do_in("if expression", token(token.RParen))
  use then <- do_in("if expression", pratt.sub_expression(config, 0))
  use otherwise <- do_in(
    "if expression",
    optional({
      use _ <- do_in("if expression", token(token.Else))
      use otherwise <- do_in("if expression", pratt.sub_expression(config, 0))
      return(otherwise)
    }),
  )

  return(expr.If(condition:, then:, otherwise:))
}

fn parse_function_call(config) -> Parser(expr.Expr) {
  use func <- do_in("function call", pratt.sub_expression(config, 16))
  use generics <- do_in("function call", chomp.or(parse_generic_args(), []))
  use args <- do_in(
    "function call",
    delimited(
      "function call",
      token(token.LParen),
      sequence_trailing(parse_call_arg(config), token(token.Comma)),
      token(token.RParen),
    ),
  )

  return(expr.Call(func:, generics:, args:))
}

fn parse_call_arg(config) -> Parser(expr.CallArg) {
  use name <- do_in(
    "function call argument",
    optional({
      use name <- do_in("function call argument", parse_identifier())
      use _ <- do_in("function call argument", token(token.Eq))
      return(name)
    }),
  )
  use spread <- do_in(
    "function call argument",
    optional(token(token.Spread)) |> chomp.map(option.is_some),
  )
  use value <- do_in("function call argument", pratt.sub_expression(config, 0))

  return(expr.CallArg(name:, spread:, value:))
}

fn parse_member_access(config) -> Parser(expr.Expr) {
  use parent <- do_in(
    "member access expression",
    pratt.sub_expression(config, 15),
  )
  use _ <- do_in("member access expression", token(token.Dot))
  use member <- do_in("member access expression", parse_identifier())

  return(expr.MemberAccess(parent:, member:))
}

fn parse_index_access(config) -> Parser(expr.Expr) {
  use parent <- do_in(
    "index access expression",
    pratt.sub_expression(config, 14),
  )
  use index <- do_in(
    "index access expression",
    delimited(
      "index access expression",
      token(token.LBracket),
      pratt.sub_expression(config, 0),
      token(token.RBracket),
    ),
  )

  return(expr.IndexAccess(parent:, index:))
}

fn parse_return(config) -> Parser(expr.Expr) {
  use _ <- do_in("return expression", token(token.Return))
  use label <- do_in(
    "return expression",
    optional({
      use _ <- do_in("return label", token(token.AtSign))
      use label <- do_in("return label", parse_identifier())
      return(label)
    }),
  )
  use value <- do_in("return expression", pratt.sub_expression(config, 0))

  return(expr.Return(value, label))
}

fn parse_break(config) -> Parser(expr.Expr) {
  use _ <- do_in("break expression", token(token.Break))
  use label <- do_in(
    "break expression",
    optional({
      use _ <- do_in("break label", token(token.AtSign))
      use label <- do_in("break label", parse_identifier())
      return(label)
    }),
  )
  use value <- do_in("break expression", pratt.sub_expression(config, 0))

  return(expr.Break(value, label))
}

fn parse_continue(_) -> Parser(expr.Expr) {
  use _ <- do_in("continue expression", token(token.Continue))
  use label <- do_in(
    "continue expression",
    optional({
      use _ <- do_in("continue label", token(token.AtSign))
      use label <- do_in("continue label", parse_identifier())
      return(label)
    }),
  )

  return(expr.Continue(label))
}

fn parse_type_op(config) -> Parser(expr.Expr) {
  use lhs <- do_in(
    "type operation expression",
    pratt.sub_expression(config, 12),
  )
  use op <- do_in(
    "type operation expression",
    chomp.one_of([
      token(token.As) |> chomp.replace(operators.As),
      token(token.Is) |> chomp.replace(operators.Is),
      token(token.NotIs) |> chomp.replace(operators.NotIs),
    ]),
  )
  use rhs <- do_in("type operation expression", parse_type())

  return(expr.TypeOp(lhs:, op:, rhs:))
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
          "list pattern",
          token(token.ListStart),
          sequence_trailing(pratt.sub_expression(config, 0), token(token.Comma)),
          token(token.RBracket),
        )
        |> chomp.map(expr.ListPattern)
      },
      fn(config) {
        delimited(
          "tuple pattern",
          token(token.TupleStart),
          sequence_trailing(pratt.sub_expression(config, 0), token(token.Comma)),
          token(token.RParen),
        )
        |> chomp.map(expr.TuplePattern)
      },
      fn(config) {
        delimited(
          "map pattern",
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
          "grouped pattern",
          token(token.LParen),
          pratt.sub_expression(config, 0) |> chomp.map(expr.GroupedPattern),
          token(token.RParen),
        )
      },
    ],
    and_then: [pratt.infix_left(1, token(token.Pipe), expr.DisjunctionPattern)],
    or_error: "Expected pattern",
  )
}

fn parse_generic_args() -> Parser(List(types.GenericArg)) {
  delimited(
    "generic argument list",
    token(token.Lt),
    sequence_trailing(
      {
        use name <- do_in(
          "generic argument",
          optional({
            use it <- do_in("generic argument", parse_identifier())
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
}

fn parse_visibility() -> Parser(declaration.Visibility) {
  chomp.one_of([
    token(token.Public) |> chomp.replace(declaration.Public),
    token(token.Internal) |> chomp.replace(declaration.Internal),
    token(token.Private) |> chomp.replace(declaration.Private),
  ])
}

fn parse_identifier() -> Parser(String) {
  use t <- do_in("identifier", chomp.any())

  case t {
    token.Identifier(name) -> return(name)
    _ -> fail("Expected an identifier")
  }
}

fn infix_op(
  precedence: Int,
  token: token.NymphToken,
  op: b,
  constructor: fn(a, b, a) -> a,
  left_assoc: Bool,
) {
  case left_assoc {
    True ->
      pratt.infix_left(precedence, chomp.token(token), fn(lhs, rhs) {
        constructor(lhs, op, rhs)
      })
    False ->
      pratt.infix_right(precedence, chomp.token(token), fn(lhs, rhs) {
        constructor(lhs, op, rhs)
      })
  }
}

fn delimited(
  ctx: String,
  start: Parser(_),
  parser: Parser(a),
  end: Parser(_),
) -> Parser(a) {
  use _ <- do_in(ctx, start)
  use it <- do_in(ctx, parser)
  use _ <- do_in(ctx, end)
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
