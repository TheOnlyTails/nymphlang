import chomp.{run}
import chomp/lexer as chomp_lexer
import gleam/int
import gleam/io
import nymph/lexer
import nymph/parser
import pprint

pub fn main() {
  let source = "let x = 1 + foo"

  let tokens = case lexer.run(source) {
    Ok(tokens) -> tokens |> pprint.debug
    Error(chomp_lexer.NoMatchFound(row:, col:, lexeme:)) -> {
      io.println_error(
        "No match found for lexeme "
        <> lexeme
        <> "at location"
        <> int.to_string(row)
        <> ":"
        <> int.to_string(col),
      )
      panic
    }
  }

  run(tokens, parser.parser()) |> pprint.debug
}
