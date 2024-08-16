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
  Normal
  String
}

fn reserved() -> set.Set(String) {
  [
    "true", "false", "public", "internal", "private", "import", "with", "type",
    "struct", "enum", "impl", "interface", "namespace", "for", "while", "match",
    "if", "else", "int", "float", "char", "string", "boolean", "void", "never",
    "as", "is", "in", "continue", "break", "return", "this", "_",
  ]
  |> set.from_list
}

pub fn lexer() -> lexer.Lexer(NymphToken, LexMode) {
  lexer.advanced(fn(mode) {
    case mode {
      Normal -> [
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
          "[$_\\p{XID_Start}]",
          "[\\p{XID_Continue}]",
          reserved(),
          token.Identifier,
        ),
        // keywords and punctuation
        lexer.keyword("true", "\\P{XID_Start}", token.True),
        lexer.keyword("false", "\\P{XID_Start}", token.False),
        lexer.keyword("public", "\\P{XID_Start}", token.Public),
        lexer.keyword("internal", "\\P{XID_Start}", token.Internal),
        lexer.keyword("private", "\\P{XID_Start}", token.Private),
        lexer.keyword("import", "\\P{XID_Start}", token.Import),
        lexer.keyword("with", "\\P{XID_Start}", token.With),
        lexer.token("(", token.LParen),
        lexer.token(")", token.RParen),
        lexer.token("[", token.LBracket),
        lexer.token("]", token.RBracket),
        lexer.token("{", token.LBrace),
        lexer.token("}", token.RBrace),
        lexer.keyword("type", "\\P{XID_Start}", token.Type),
        lexer.keyword("struct", "\\P{XID_Start}", token.Struct),
        lexer.keyword("enum", "\\P{XID_Start}", token.Enum),
        lexer.keyword("let", "\\P{XID_Start}", token.Let),
        lexer.keyword("mut", "\\P{XID_Start}", token.Mut),
        lexer.keyword("func", "\\P{XID_Start}", token.Func),
        lexer.keyword("interface", "\\P{XID_Start}", token.Interface),
        lexer.keyword("impl", "\\P{XID_Start}", token.Impl),
        lexer.keyword("namespace", "\\P{XID_Start}", token.Namespace),
        lexer.keyword("for", "\\P{XID_Start}", token.For),
        lexer.keyword("while", "\\P{XID_Start}", token.While),
        lexer.keyword("if", "\\P{XID_Start}", token.If),
        lexer.keyword("else", "\\P{XID_Start}", token.Else),
        lexer.keyword("match", "\\P{XID_Start}", token.Match),
        lexer.keyword("int", "\\P{XID_Start}", token.IntType),
        lexer.keyword("float", "\\P{XID_Start}", token.FloatType),
        lexer.keyword("boolean", "\\P{XID_Start}", token.BooleanType),
        lexer.keyword("char", "\\P{XID_Start}", token.CharType),
        lexer.keyword("string", "\\P{XID_Start}", token.StringType),
        lexer.keyword("void", "\\P{XID_Start}", token.VoidType),
        lexer.keyword("never", "\\P{XID_Start}", token.NeverType),
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
        lexer.keyword("_", "\\P{XID_Start}", token.Underscore),
        lexer.token("++", token.Increment),
        lexer.token("--", token.Decrement),
        lexer.keyword("!", "[^i=]", token.ExclamationMark),
        lexer.keyword("+", "[^+=]", token.Plus),
        lexer.keyword("-", "[^-=]", token.Minus),
        lexer.keyword("*", "[^*=]", token.Star),
        lexer.keyword("/", "[^/=]", token.Slash),
        lexer.keyword("%", "[^=]", token.Percent),
        lexer.keyword("**", "[^=]", token.StarStar),
        lexer.keyword("&", "[^&=]", token.And),
        lexer.keyword("|", "[^|=]", token.Pipe),
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
        lexer.keyword("as", "\\P{XID_Start}", token.As),
        lexer.keyword("is", "\\P{XID_Start}", token.Is),
        lexer.keyword("!is", "\\P{XID_Start}", token.NotIs),
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
        lexer.keyword("continue", "\\P{XID_Start}", token.Continue),
        lexer.keyword("break", "\\P{XID_Start}", token.Break),
        lexer.keyword("return", "\\P{XID_Start}", token.Return),
        lexer.keyword("this", "\\P{XID_Start}", token.This),
        lexer.comment("//", token.Comment),
        lexer.spaces_(token.Whitespace),
      ]
      String -> []
    }
  })
}

fn number_lexer(
  base: Int,
  digit: String,
  prefix: String,
  token: fn(Int) -> NymphToken,
) -> lexer.Matcher(NymphToken, LexMode) {
  use text <- then_try(regex_lexer_options(
    "^" <> prefix <> "(?!0)" <> digit <> "(_?" <> digit <> ")*$",
    "[" <> prefix <> "]|" <> digit <> "|_",
    regex.Options(case_insensitive: True, multi_line: False),
  ))

  text
  |> string.drop_left(prefix |> string.length)
  |> string.replace("_", "")
  |> int.base_parse(base)
  |> result.map(fn(val) { #(token(val), Normal) })
}

fn float_both_lexer() -> lexer.Matcher(NymphToken, LexMode) {
  let assert Ok(separator) = regex.from_string("[eE]")
  use text <- then_try(regex_lexer(
    "\\d(_?\\d)*\\.\\d(_?\\d)*[eE][-+]?\\d(_?\\d)*",
    "[\\d_\\.eE+-]",
  ))

  let assert [mantissa, exponent] = regex.split(separator, text)
  use mantissa <- result.try(mantissa |> string.replace("_", "") |> float.parse)
  use exponent <- result.try(exponent |> exponent_lexer)
  use value <- result.try(float.power(mantissa, int.to_float(exponent)))

  Ok(#(token.FloatBothParts(value), Normal))
}

fn float_first_lexer() -> lexer.Matcher(NymphToken, LexMode) {
  let assert Ok(separator) = regex.from_string("[eE]")
  use text <- then_try(regex_lexer(
    "\\d(_?\\d)*[eE][-+]?\\d(_?\\d)*",
    "[\\d_eE+-]",
  ))

  let assert [mantissa, exponent] = regex.split(separator, text)
  use mantissa <- result.try(mantissa |> string.replace("_", "") |> int.parse)
  use exponent <- result.try(exponent |> exponent_lexer)
  use value <- result.try(int.power(mantissa, int.to_float(exponent)))

  Ok(#(token.FloatFirstPart(value), Normal))
}

fn float_second_lexer() -> lexer.Matcher(NymphToken, LexMode) {
  let assert Ok(separator) = regex.from_string("[eE]")
  use text <- then_try(regex_lexer(
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

  Ok(#(token.FloatFirstPart(value), Normal))
}

fn float_no_exp_lexer() {
  // this is a crazy regex that enforces the following rules:
  // - no zeroes at the start of the decimal portion
  // - no separators at the start, end, or next to the decimal point
  // - either the decimal or the fractional portions may be missing, but not both
  use text <- then_try(regex_lexer(
    "^(0|[1-9](_?\\d)*)\\.\\d*(_?\\d)*|\\.\\d+(_\\d*)*$",
    "[\\d_\\.]",
  ))

  string.replace(text, "_", "")
  |> string.append("0")
  |> string.append("0", suffix: _)
  |> float.parse()
  |> result.map(fn(val) { #(token.FloatNoExponent(val), Normal) })
}

fn float_int_lexer() {
  use text <- then_try(regex_lexer("\\d(_?\\d)*f", "[\\d_f]"))

  string.replace(text, "_", "")
  |> string.drop_right(1)
  |> int.parse
  |> result.map(int.to_float)
  |> result.map(fn(val) { #(token.FloatInt(val), Normal) })
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
  use text <- then_try(regex_lexer("'\\\\[nNrRtT'\\\\]'", "[\\\\nNrRtT']"))

  case text |> string.drop_left(2) |> string.drop_right(1) {
    "n" | "N" -> Ok(#(token.CharEscape(expr.Newline), Normal))
    "r" | "R" -> Ok(#(token.CharEscape(expr.Carriage), Normal))
    "t" | "T" -> Ok(#(token.CharEscape(expr.Tab), Normal))
    "\\" -> Ok(#(token.CharEscape(expr.Backslash), Normal))
    "'" -> Ok(#(token.CharEscape(expr.Apostrophe), Normal))
    _ -> Error(Nil)
  }
}

fn char_unicode() {
  use text <- then_try(regex_lexer(
    "'\\\\u[a-fA-F\\d]{1,6}'",
    "[\\\\ua-fA-F\\d']",
  ))

  text
  |> string.drop_left(2)
  |> int.base_parse(16)
  |> result.map(fn(val) { #(token.CharUnicode(val), Normal) })
}

fn char_regular() {
  use text <- then_try(regex_lexer("'[^\\n'\\\\]'", "[^\\n\\\\]"))

  text
  |> string.slice(at_index: 1, length: 1)
  |> string.to_utf_codepoints
  |> list.first
  |> result.map(fn(val) { #(token.Char(val), Normal) })
}

fn then_try(
  matcher: lexer.Matcher(a, mode),
  f: fn(a) -> Result(#(c, mode), Nil),
) -> lexer.Matcher(c, mode) {
  use match <- lexer.then(matcher)

  case f(match) {
    Ok(#(res, mode)) -> lexer.Keep(res, mode)
    Error(_) -> lexer.NoMatch
  }
}

fn regex_lexer_options(check: String, stop: String, options: regex.Options) {
  let assert Ok(check) = regex.compile(check, options)
  let assert Ok(stop) = regex.compile(stop, options)
  use mode, lexeme, lookahead <- lexer.custom

  case !regex.check(stop, lookahead) && regex.check(check, lexeme) {
    True -> lexer.Keep(lexeme, mode)
    False -> lexer.NoMatch
  }
}

fn regex_lexer(check: String, stop: String) {
  regex_lexer_options(
    check,
    stop,
    regex.Options(case_insensitive: False, multi_line: False),
  )
}
