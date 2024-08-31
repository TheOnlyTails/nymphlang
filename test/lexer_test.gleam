import chomp/lexer.{Token} as chomp_lexer
import chomp/span.{Span}
import gleeunit/should
import nymph/parser/lexer
import nymph/parser/token

pub fn decimal_int_test() {
  chomp_lexer.run_advanced("0", lexer.Normal(0), lexer.lexer())
  |> should.be_ok()
  |> should.equal([Token(Span(1, 1, 1, 2), "0", token.DecimalInt(0))])

  // numbers must be at least one digit
  chomp_lexer.run_advanced("1", lexer.Normal(0), lexer.lexer())
  |> should.be_ok()
  |> should.equal([Token(Span(1, 1, 1, 2), "1", token.DecimalInt(1))])

  // numbers can have multiple digits
  chomp_lexer.run_advanced("1109234571", lexer.Normal(0), lexer.lexer())
  |> should.be_ok()
  |> should.equal([
    Token(Span(1, 1, 1, 11), "1109234571", token.DecimalInt(1_109_234_571)),
  ])

  // numbers can have underscores as digit separators
  chomp_lexer.run_advanced("1_00_0_00_1", lexer.Normal(0), lexer.lexer())
  |> should.be_ok()
  |> should.equal([
    Token(Span(1, 1, 1, 12), "1_00_0_00_1", token.DecimalInt(1_000_001)),
  ])

  // numbers may not end with a digit separator
  chomp_lexer.run_advanced("1_", lexer.Normal(0), lexer.lexer())
  |> should.be_error()
  |> should.equal(chomp_lexer.NoMatchFound(1, 1, "1_"))
}

pub fn radix_int_test() {
  chomp_lexer.run_advanced("b0", lexer.Normal(0), lexer.lexer())
  |> should.be_ok()
  |> should.equal([Token(Span(1, 1, 1, 3), "b0", token.BinaryInt(0b0))])

  // binary numbers are prefixed with b
  chomp_lexer.run_advanced("b10101001", lexer.Normal(0), lexer.lexer())
  |> should.be_ok()
  |> should.equal([
    Token(Span(1, 1, 1, 10), "b10101001", token.BinaryInt(0b10101001)),
  ])
  // octal numbers are prefixed with o
  chomp_lexer.run_advanced("o711413745", lexer.Normal(0), lexer.lexer())
  |> should.be_ok()
  |> should.equal([
    Token(Span(1, 1, 1, 11), "o711413745", token.OctalInt(0o711413745)),
  ])
  // hexadecimal numbers are prefixed with x
  chomp_lexer.run_advanced("xDEADF00D", lexer.Normal(0), lexer.lexer())
  |> should.be_ok()
  |> should.equal([
    Token(Span(1, 1, 1, 10), "xDEADF00D", token.HexInt(0xDEADF00D)),
  ])

  // numbers are case insensitive
  chomp_lexer.run_advanced("xDEaDf00d", lexer.Normal(0), lexer.lexer())
  |> should.be_ok()
  |> should.equal([
    Token(Span(1, 1, 1, 10), "xDEaDf00d", token.HexInt(0xDEADF00D)),
  ])
}
