import chomp.{run}
import chomp/lexer as chomp_lexer
import dedent
import gleam/int
import gleam/io
import hug
import nymph/compile
import nymph/parser
import nymph/parser/lexer
import nymph/parser/token
import pprint

pub fn main() {
  let module_path = "stdin"
  let source =
    "
    enum BinaryTree<T> {
      Leaf { value: T },
      Node { left: BinaryTree<T>, right: BinaryTree<T> },
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

  let module = case run(tokens, parser.parser(module_path)) {
    Ok(mod) -> mod |> pprint.debug
    Error(#(error, span, _)) -> {
      hug.error(
        in: module_path,
        containing: source,
        from: #(span.row_start, span.col_start),
        to: #(span.row_end, span.col_end),
        message: case error {
          chomp.Custom(error) -> error
          chomp.EndOfInput -> "Unexpected end of input!"
          chomp.Expected(expected, got) ->
            "Expected to find "
            <> token.to_string(expected)
            <> ", got "
            <> token.to_string(got)
            <> "instead"
          chomp.Unexpected(got) -> "Unexpected " <> token.to_string(got)
          chomp.BadParser(error) -> "Internal error: " <> error
        },
        hint: "",
      )
      |> io.println_error
      panic
    }
  }

  compile.compile(module).1 |> io.println
}
