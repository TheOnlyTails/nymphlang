import chomp/lexer as chomp
import nymph/lexer
import pprint

pub fn main() {
  let lex = lexer.lexer()

  chomp.run_advanced("\"${\"${$}\"}\"", lexer.Normal(0), lex) |> pprint.debug
}
