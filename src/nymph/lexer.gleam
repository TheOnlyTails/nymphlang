import chomp/lexer
import gleam/bool
import gleam/float
import gleam/int
import gleam/list
import gleam/regex
import gleam/result
import gleam/set
import gleam/string
import nymph/ast/expr
import nymph/token.{type NymphToken}

pub type LexMode {
  Normal(nesting: Int)
  String(nesting: Int)
}

fn reserved() -> set.Set(String) {
  [
    "true", "false", "public", "internal", "private", "import", "with", "let",
    "mut", "type", "struct", "enum", "impl", "interface", "namespace", "for",
    "while", "match", "if", "else", "int", "float", "char", "string", "boolean",
    "void", "never", "as", "is", "in", "continue", "break", "return", "this",
    "_",
  ]
  |> set.from_list
}

pub fn lexer() -> lexer.Lexer(NymphToken, LexMode) {
  lexer.advanced(fn(mode) {
    case mode {
      Normal(nesting) -> [
        // int literals
        number_lexer(2, "[01]", "0b", token.BinaryInt),
        number_lexer(8, "[0-7]", "0o", token.OctalInt),
        number_lexer(16, "[a-fA-F\\d]", "0x", token.HexInt),
        number_lexer(10, "\\d", "", token.DecimalInt),
        // float literals
        float_both_lexer(),
        float_first_lexer(),
        float_second_lexer(),
        float_no_exp_lexer(),
        float_int_lexer(),
        // char literal
        char_escape(),
        char_unicode(),
        char_regular(),
        // identifiers
        lexer.identifier(
          "[$_a-zA-Z]",
          "[$_a-zA-Z0-9]",
          reserved(),
          token.Identifier,
        ),
        // string literal
        lexer.token("\"", token.StringStart)
          |> lexer.into(fn(_) { String(nesting) }),
        // keywords and punctuation
        lexer.keyword("true", "[^$_a-zA-Z]", token.True),
        lexer.keyword("false", "[^$_a-zA-Z]", token.False),
        lexer.keyword("public", "[^$_a-zA-Z]", token.Public),
        lexer.keyword("internal", "[^$_a-zA-Z]", token.Internal),
        lexer.keyword("private", "[^$_a-zA-Z]", token.Private),
        lexer.keyword("import", "[^$_a-zA-Z]", token.Import),
        lexer.keyword("with", "[^$_a-zA-Z]", token.With),
        lexer.token("(", token.LParen),
        lexer.token(")", token.RParen),
        lexer.token("[", token.LBracket),
        lexer.token("]", token.RBracket),
        lexer.token("{", token.LBrace)
          |> lexer.into(fn(_) { Normal(nesting + 1) }),
        lexer.token("}", case nesting {
          0 -> token.StringInterpolationEnd
          _ -> token.RBrace
        })
          |> lexer.into(fn(_) {
            case nesting {
              0 -> String(nesting)
              _ -> Normal(nesting - 1)
            }
          }),
        lexer.keyword("type", "[^$_a-zA-Z]", token.Type),
        lexer.keyword("struct", "[^$_a-zA-Z]", token.Struct),
        lexer.keyword("enum", "[^$_a-zA-Z]", token.Enum),
        lexer.keyword("let", "[^$_a-zA-Z]", token.Let),
        lexer.keyword("mut", "[^$_a-zA-Z]", token.Mut),
        lexer.keyword("func", "[^$_a-zA-Z]", token.Func),
        lexer.keyword("interface", "[^$_a-zA-Z]", token.Interface),
        lexer.keyword("impl", "[^$_a-zA-Z]", token.Impl),
        lexer.keyword("namespace", "[^$_a-zA-Z]", token.Namespace),
        lexer.keyword("for", "[^$_a-zA-Z]", token.For),
        lexer.keyword("while", "[^$_a-zA-Z]", token.While),
        lexer.keyword("if", "[^$_a-zA-Z]", token.If),
        lexer.keyword("else", "[^$_a-zA-Z]", token.Else),
        lexer.keyword("match", "[^$_a-zA-Z]", token.Match),
        lexer.keyword("int", "[^$_a-zA-Z]", token.IntType),
        lexer.keyword("float", "[^$_a-zA-Z]", token.FloatType),
        lexer.keyword("boolean", "[^$_a-zA-Z]", token.BooleanType),
        lexer.keyword("char", "[^$_a-zA-Z]", token.CharType),
        lexer.keyword("string", "[^$_a-zA-Z]", token.StringType),
        lexer.keyword("void", "[^$_a-zA-Z]", token.VoidType),
        lexer.keyword("never", "[^$_a-zA-Z]", token.NeverType),
        lexer.token("#[", token.ListStart),
        lexer.token("#(", token.TupleStart),
        lexer.token("#{", token.MapStart),
        lexer.token("->", token.Arrow),
        lexer.token("...", token.Spread),
        lexer.token("?", token.QuestionMark),
        lexer.keyword(".", "[^=\\.\\d]", token.Dot),
        lexer.token("@", token.AtSign),
        lexer.token(",", token.Comma),
        lexer.token(":", token.Colon),
        lexer.keyword("_", "[^$_a-zA-Z]", token.Underscore),
        lexer.token("|>", token.Triangle),
        lexer.token("++", token.PlusPlus),
        lexer.token("--", token.MinusMinus),
        lexer.keyword("!", "[^i=]", token.ExclamationMark),
        lexer.keyword("+", "[^+=]", token.Plus),
        lexer.keyword("-", "[^-=]", token.Minus),
        lexer.keyword("*", "[^*=]", token.Star),
        lexer.keyword("/", "[^/=]", token.Slash),
        lexer.keyword("%", "[^=]", token.Percent),
        lexer.keyword("**", "[^=]", token.StarStar),
        lexer.keyword("&", "[^&=]", token.And),
        lexer.keyword("|", "[^>|=]", token.Pipe),
        lexer.keyword("^", "[^=^]", token.Caret),
        lexer.keyword("<<", "[^=]", token.LtLt),
        lexer.keyword(">>", "[^=]", token.GtGt),
        lexer.token("==", token.EqEq),
        lexer.token("!=", token.NotEq),
        lexer.keyword("<", "[^=]", token.Lt),
        lexer.keyword(">", "[^=]", token.Gt),
        lexer.token("<=", token.LtEq),
        lexer.token(">=", token.GtEq),
        lexer.token("in", token.In),
        lexer.token("!in", token.NotIn),
        lexer.keyword("&&", "[^=]", token.AndAnd),
        lexer.keyword("||", "[^=]", token.PipePipe),
        lexer.keyword("as", "[^$_a-zA-Z]", token.As),
        lexer.keyword("is", "[^$_a-zA-Z]", token.Is),
        lexer.keyword("!is", "[^$_a-zA-Z]", token.NotIs),
        lexer.keyword("=", "[^=]", token.Eq),
        lexer.token("+=", token.PlusEq),
        lexer.token("-=", token.MinusEq),
        lexer.token("*=", token.StarEq),
        lexer.token("/=", token.SlashEq),
        lexer.token("%=", token.PercentEq),
        lexer.token("**=", token.StarStarEq),
        lexer.token("&&=", token.AndAndEq),
        lexer.token("||=", token.PipePipeEq),
        lexer.token("&=", token.AndEq),
        lexer.token("|=", token.PipeEq),
        lexer.token("^=", token.CaretEq),
        lexer.token("<<=", token.LtLtEq),
        lexer.token(">>=", token.GtGtEq),
        lexer.token("..", token.DotDot),
        lexer.token("..=", token.DotDotEq),
        lexer.keyword("continue", "[^$_a-zA-Z]", token.Continue),
        lexer.keyword("break", "[^$_a-zA-Z]", token.Break),
        lexer.keyword("return", "[^$_a-zA-Z]", token.Return),
        lexer.keyword("this", "[^$_a-zA-Z]", token.This),
        lexer.comment("//", token.Comment),
        lexer.spaces_(token.Whitespace),
      ]
      String(nesting) -> [
        lexer.token("\"", token.StringEnd)
          |> lexer.into(fn(_) { Normal(nesting) }),
        lexer.token("${", token.StringInterpolationStart)
          |> lexer.into(fn(_) { Normal(nesting) }),
        string_unicode(),
        string_escape(),
        string_regular(),
      ]
    }
  })
}

pub fn run(source: String) {
  lexer.run_advanced(source, Normal(0), lexer())
  |> result.map(list.filter(_, keeping: fn(tok: lexer.Token(NymphToken)) {
    case tok.value {
      token.Comment(_) | token.CommentMultiline(_) | token.Whitespace(_) ->
        False
      _ -> True
    }
  }))
}

fn number_lexer(
  base: Int,
  digit: String,
  prefix: String,
  token: fn(Int) -> NymphToken,
) -> lexer.Matcher(NymphToken, LexMode) {
  use text, mode <- then_try(regex_lexer_options(
    "^" <> prefix <> "(?!0)" <> digit <> "(_?" <> digit <> ")*$",
    case string.is_empty(prefix) {
      False -> "[" <> prefix <> "]|"
      True -> ""
    }
      <> digit
      <> "|_",
    regex.Options(case_insensitive: True, multi_line: False),
  ))

  text
  |> string.drop_left(prefix |> string.length)
  |> string.replace("_", "")
  |> int.base_parse(base)
  |> result.map(fn(val) { #(token(val), mode) })
}

fn float_both_lexer() -> lexer.Matcher(NymphToken, LexMode) {
  let assert Ok(separator) = regex.from_string("[eE]")
  use text, mode <- then_try(regex_lexer(
    "\\d(_?\\d)*\\.\\d(_?\\d)*[eE][-+]?\\d(_?\\d)*",
    "[\\d_\\.eE+-]",
  ))

  let assert [mantissa, exponent] = regex.split(separator, text)
  use mantissa <- result.try(mantissa |> string.replace("_", "") |> float.parse)
  use exponent <- result.try(exponent |> exponent_lexer)
  use value <- result.try(float.power(mantissa, int.to_float(exponent)))

  Ok(#(token.FloatBothParts(value), mode))
}

fn float_first_lexer() -> lexer.Matcher(NymphToken, LexMode) {
  let assert Ok(separator) = regex.from_string("[eE]")
  use text, mode <- then_try(regex_lexer(
    "\\d(_?\\d)*[eE][-+]?\\d(_?\\d)*",
    "[\\d_eE+-]",
  ))

  let assert [mantissa, exponent] = regex.split(separator, text)
  use mantissa <- result.try(mantissa |> string.replace("_", "") |> int.parse)
  use exponent <- result.try(exponent |> exponent_lexer)
  use value <- result.try(int.power(mantissa, int.to_float(exponent)))

  Ok(#(token.FloatFirstPart(value), mode))
}

fn float_second_lexer() -> lexer.Matcher(NymphToken, LexMode) {
  let assert Ok(separator) = regex.from_string("[eE]")
  use text, mode <- then_try(regex_lexer(
    "\\.\\d(_?\\d)*[eE][-+]?\\d(_?\\d)*",
    "[\\d_\\.eE+-]",
  ))

  let assert [mantissa, exponent] = regex.split(separator, text)
  use mantissa <- result.try(
    mantissa
    |> string.append("0", suffix: _)
    |> string.replace("_", "")
    |> int.parse,
  )
  use exponent <- result.try(exponent |> exponent_lexer)
  use value <- result.try(int.power(mantissa, int.to_float(exponent)))

  Ok(#(token.FloatFirstPart(value), mode))
}

fn float_no_exp_lexer() {
  // this is a crazy regex that enforces the following rules:
  // - no zeroes at the start of the decimal portion
  // - no separators at the start, end, or next to the decimal point
  // - either the decimal or the fractional portions may be missing, but not both
  use text, mode <- then_try(regex_lexer(
    "^(0|[1-9](_?\\d)*)\\.\\d*(_?\\d)*|\\.\\d+(_\\d*)*$",
    "[\\d_\\.]",
  ))

  string.replace(text, "_", "")
  |> string.append("0")
  |> string.append("0", suffix: _)
  |> float.parse()
  |> result.map(fn(val) { #(token.FloatNoExponent(val), mode) })
}

fn float_int_lexer() {
  use text, mode <- then_try(regex_lexer("\\d(_?\\d)*f", "[\\d_f]"))

  string.replace(text, "_", "")
  |> string.drop_right(1)
  |> int.parse
  |> result.map(int.to_float)
  |> result.map(fn(val) { #(token.FloatInt(val), mode) })
}

fn exponent_lexer(exponent: String) {
  let #(explicit, sign) = case exponent |> string.first() {
    Ok("-") -> #(True, -1)
    Ok("+") -> #(True, 1)
    _ -> #(False, 1)
  }

  exponent
  |> string.drop_left(bool.to_int(explicit))
  |> string.replace("_", "")
  |> int.parse
  |> result.map(int.multiply(_, sign))
}

fn char_escape() {
  use text, mode <- then_try(regex_lexer("'\\\\[nNrRtT'\\\\]'", "[\\\\nNrRtT']"))

  case text |> string.drop_left(2) |> string.drop_right(1) {
    "n" | "N" -> Ok(#(token.CharEscape(expr.Newline), mode))
    "r" | "R" -> Ok(#(token.CharEscape(expr.Carriage), mode))
    "t" | "T" -> Ok(#(token.CharEscape(expr.Tab), mode))
    "\\" -> Ok(#(token.CharEscape(expr.Backslash), mode))
    "'" -> Ok(#(token.CharEscape(expr.Apostrophe), mode))
    _ -> Error(Nil)
  }
}

fn char_unicode() {
  use text, mode <- then_try(regex_lexer(
    "'\\\\u[a-fA-F\\d]{1,6}'",
    "[\\\\ua-fA-F\\d']",
  ))

  text
  |> string.drop_left(3)
  |> string.drop_right(1)
  |> int.base_parse(16)
  |> result.map(fn(val) { #(token.CharUnicode(val), mode) })
}

fn char_regular() {
  use text, mode <- then_try(regex_lexer("'[^\\n'\\\\]'", "[^\\n\\\\]"))

  text
  |> string.slice(at_index: 1, length: 1)
  |> string.to_utf_codepoints
  |> list.first
  |> result.map(fn(val) { #(token.Char(val), mode) })
}

fn string_escape() {
  use text, mode <- then_try(regex_lexer(
    "\\\\([nNrRtT\"\\\\]|\\$\\{)",
    "[\\\\nNrRtT${\"]",
  ))

  case text |> string.drop_left(1) {
    "n" | "N" -> Ok(#(token.StringEscape(expr.Newline), mode))
    "r" | "R" -> Ok(#(token.StringEscape(expr.Carriage), mode))
    "t" | "T" -> Ok(#(token.StringEscape(expr.Tab), mode))
    "\\" -> Ok(#(token.StringEscape(expr.Backslash), mode))
    "\"" -> Ok(#(token.StringEscape(expr.Quote), mode))
    "${" -> Ok(#(token.StringEscape(expr.Interpolation), mode))
    _ -> Error(Nil)
  }
}

fn string_unicode() {
  use text, mode <- then_try(regex_lexer(
    "\\\\u[a-fA-F\\d]{1,6}",
    "[\\\\ua-fA-F\\d]",
  ))

  text
  |> string.drop_left(2)
  |> int.base_parse(16)
  |> result.map(fn(val) { #(token.StringUnicode(val), mode) })
}

fn string_regular() {
  use mode, lexeme, lookahead <- lexer.custom

  case lexeme, lookahead {
    "$", "{" | "\\", _ | "\"", _ -> lexer.NoMatch
    _, _ ->
      lexeme
      |> string.to_utf_codepoints
      |> list.first
      |> result.map(fn(val) { lexer.Keep(token.StringChar(val), mode) })
      |> result.unwrap(or: lexer.NoMatch)
  }
}

fn then_try(
  matcher: fn(mode, String, String) -> lexer.Match(a, mode),
  f: fn(a, mode) -> Result(#(c, mode), Nil),
) -> lexer.Matcher(c, mode) {
  use mode, lexeme, lookahead <- lexer.custom

  case matcher(mode, lexeme, lookahead) {
    lexer.Keep(val, mode) ->
      case f(val, mode) {
        Ok(#(val, mode)) -> lexer.Keep(val, mode)
        Error(_) -> lexer.NoMatch
      }
    lexer.Skip -> lexer.Skip
    lexer.Drop(mode) -> lexer.Drop(mode)
    lexer.NoMatch -> lexer.NoMatch
  }
}

fn regex_lexer_options(check: String, stop: String, options: regex.Options) {
  let assert Ok(check) = regex.compile(check, options)
  let assert Ok(stop) = regex.compile(stop, options)

  fn(mode, lexeme, lookahead) {
    case !regex.check(stop, lookahead) && regex.check(check, lexeme) {
      True -> lexer.Keep(lexeme, mode)
      False -> lexer.NoMatch
    }
  }
}

fn regex_lexer(check: String, stop: String) {
  regex_lexer_options(
    check,
    stop,
    regex.Options(case_insensitive: False, multi_line: False),
  )
}
