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
    enum Option<T> {
      Some(value: T),
      None,

      func is_some() -> match (this) {
        Some { ... } -> true,
        None -> false,
      }

      func map<R>(f: (T) -> R) -> match (this) {
        Some { value } -> Some(f(value)),
        None -> None
      }
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
