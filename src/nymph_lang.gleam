import chomp.{run}
import chomp/lexer as chomp_lexer
import dedent
import gleam/int
import gleam/io
import nymph/parser
import nymph/parser/lexer
import pprint

pub fn main() {
  let source =
    "
    func factorial(n: int) -> match (n) {
      ..=1 -> 1,
      n -> n * factorial(n - 1)
    }
    "
    |> dedent.dedent

  let tokens = case lexer.run(source) {
    Ok(tokens) -> tokens |> pprint.debug
    Error(chomp_lexer.NoMatchFound(row:, col:, lexeme:)) -> {
      io.println_error(
        "No match found for lexeme ("
        <> int.to_string(row)
        <> ":"
        <> int.to_string(col)
        <> ")\n"
        <> lexeme,
      )
      panic
    }
  }

  run(tokens, parser.parser()) |> pprint.debug
}
