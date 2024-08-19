import chomp.{do, fail, optional, return, token}
import chomp/pratt
import gleam/dict
import gleam/int
import gleam/option
import gleam/string
import nymph/ast/declaration
import nymph/ast/expr
import nymph/ast/operators
import nymph/ast/types
import nymph/token

type Parser(a) =
  chomp.Parser(a, String, token.NymphToken, Nil)

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
  use visibility <- do(optional(parse_visibility()))
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

  declaration.Let(
    declaration.LetDeclaration(visibility:, mutable:, name:, type_:),
    value,
  )
  |> return
}

fn parse_import() {
  let import_ident =
    chomp.lazy(fn() {
      use name <- do(parse_identifier())
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
      use _ <- do(token(token.LParen))
      use idents <- do(sequence_trailing(import_ident, token(token.Comma)))
      use _ <- do(token(token.RParen))

      dict.from_list(idents) |> return
    })
  use _ <- do(token(token.Import))
  use path <- do(sequence_trailing(parse_identifier(), token(token.Slash)))
  use idents <- do(chomp.optional(with_clause))

  return(declaration.Import(path, idents))
}

fn parse_expr() {
  pratt.expression(
    one_of: [
      // function call
      fn(config) {
        let call_arg =
          chomp.lazy(fn() {
            use name <- do(
              optional({
                use name <- do(parse_identifier())
                use _ <- do(token(token.Eq))
                return(name)
              }),
            )
            use spread <- do(
              optional(token(token.Spread)) |> chomp.map(option.is_some),
            )
            use value <- do(pratt.sub_expression(config, 0))

            return(expr.CallArg(name:, spread:, value:))
          })
        use func <- do(pratt.sub_expression(config, 15))
        use generics <- do(chomp.or(parse_generic_args(), []))
        use args <- do(delimited(
          token(token.LParen),
          sequence_trailing(call_arg, token(token.Comma)),
          token(token.RParen),
        ))

        return(expr.Call(func:, generics:, args:))
      },
      // negate number
      pratt.prefix(12, token(token.Minus), expr.PrefixOp(operators.Negate, _)),
      // boolean NOT
      pratt.prefix(12, token(token.ExclamationMark), expr.PrefixOp(
        operators.Not,
        _,
      )),
      fn(_) { parse_int() |> chomp.map(expr.Int) },
      fn(_) { parse_float() |> chomp.map(expr.Float) },
      fn(_) { parse_char() |> chomp.map(expr.Char) },
      fn(_) { parse_boolean() |> chomp.map(expr.Boolean) },
      fn(config) { parse_string(config) |> chomp.map(expr.String) },
    ],
    and_then: [
      pratt.postfix(12, token(token.PlusPlus), expr.PostfixOp(
        operators.Increment,
        _,
      )),
      pratt.postfix(12, token(token.MinusMinus), expr.PostfixOp(
        operators.Decrement,
        _,
      )),
      pratt.infix_right(11, token(token.StarStar), fn(left, right) {
        expr.BinaryOp(left, operators.Power, right)
      }),
      pratt.infix_right(10, token(token.Star), fn(left, right) {
        expr.BinaryOp(left, operators.Times, right)
      }),
      pratt.infix_right(10, token(token.Slash), fn(left, right) {
        expr.BinaryOp(left, operators.Divide, right)
      }),
      pratt.infix_right(10, token(token.Percent), fn(left, right) {
        expr.BinaryOp(left, operators.Modulus, right)
      }),
      pratt.infix_right(9, token(token.Plus), fn(left, right) {
        expr.BinaryOp(left, operators.Plus, right)
      }),
      pratt.infix_right(9, token(token.Minus), fn(left, right) {
        expr.BinaryOp(left, operators.Minus, right)
      }),
    ],
    or_error: "Expected expression",
  )
  |> chomp.backtrackable
}

fn parse_type() {
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
          token(token.ListStart),
          pratt.sub_expression(config, 0),
          token(token.RBracket),
        )
        |> chomp.map(types.ListType)
      },
      // map type
      fn(config) {
        delimited(
          token(token.MapStart),
          {
            use key <- do(pratt.sub_expression(config, 0))
            use _ <- do(token(token.Colon))
            use value <- do(pratt.sub_expression(config, 0))
            return(types.MapType(key, value))
          },
          token(token.RBrace),
        )
      },
      // tuple type
      fn(config) {
        delimited(
          token(token.TupleStart),
          sequence_trailing(pratt.sub_expression(config, 0), token(token.Comma)),
          token(token.RParen),
        )
        |> chomp.map(types.TupleType)
      },
      // function type
      fn(config) {
        use params <- do(delimited(
          token(token.LParen),
          sequence_trailing(pratt.sub_expression(config, 0), token(token.Comma)),
          token(token.RParen),
        ))
        use _ <- do(token(token.Arrow))
        use return_type <- do(pratt.sub_expression(config, 1))

        return(types.FuncType(params:, return_type:))
      },
      // reference type
      fn(_) {
        use name <- do(parse_identifier())
        use generics <- do(chomp.or(parse_generic_args(), []))

        return(types.Reference(name:, generics:))
      },
    ],
    and_then: [pratt.infix_left(1, token(token.Plus), types.Intersection)],
    or_error: "Expected type",
  )
  |> chomp.backtrackable
}

fn parse_int() -> Parser(Int) {
  use t <- do(chomp.any())

  case t {
    token.BinaryInt(value)
    | token.OctalInt(value)
    | token.HexInt(value)
    | token.DecimalInt(value) -> return(value)
    _ -> fail("Expected an integer literal")
  }
}

fn parse_float() -> Parser(Float) {
  use t <- do(chomp.any())

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
  use t <- do(chomp.any())

  case t {
    token.True -> return(True)
    token.False -> return(False)
    _ -> fail("Expected `true` or `false`")
  }
}

fn parse_char() -> Parser(UtfCodepoint) {
  use t <- do(chomp.any())

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
            use value <- do(pratt.sub_expression(config, 0))
            use _ <- do(token(token.StringInterpolationEnd))
            return(expr.InterpolatedExpr(value))
          }
          _ ->
            panic as "Unexpected token in string literal, should be prevented in lexing"
        }
      },
    ]),
  )
  |> delimited(token(token.StringStart), _, token(token.StringEnd))
}

fn parse_generic_args() -> Parser(List(types.GenericArg)) {
  delimited(
    token(token.Lt),
    sequence_trailing(
      {
        use name <- do(
          optional({
            use it <- do(parse_identifier())
            use _ <- do(token(token.Eq))
            return(it)
          }),
        )
        use value <- do(parse_type())

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
  use t <- do(chomp.any())

  case t {
    token.Identifier(name) -> return(name)
    _ -> fail("Expected an identifier")
  }
}

fn delimited(start: Parser(_), parser: Parser(a), end: Parser(_)) -> Parser(a) {
  use _ <- do(start)
  use it <- do(parser)
  use _ <- do(end)
  return(it)
}

fn sequence_trailing(
  parser: chomp.Parser(a, e, tok, ctx),
  separator sep: chomp.Parser(x, e, tok, ctx),
) {
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
