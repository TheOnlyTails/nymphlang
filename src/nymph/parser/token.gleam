import nymph/ast/expr

pub type NymphToken {
  /// 0b1101001
  BinaryInt(value: Int)
  /// 0o7165341
  OctalInt(value: Int)
  /// 0xDEADF00D
  HexInt(value: Int)
  /// 1234
  DecimalInt(value: Int)

  /// 1.2e3
  FloatBothParts(value: Float)
  /// 1e3
  FloatFirstPart(value: Float)
  /// .2e3
  FloatSecondPart(value: Float)
  /// 1f
  FloatInt(value: Float)
  /// 1.2
  FloatNoExponent(value: Float)

  /// 'a'
  Char(value: UtfCodepoint)
  /// '\r'
  CharEscape(value: expr.EscapeSequence)
  /// '\uDEAD'
  CharUnicode(value: Int)

  // String tokens, only matched when the lexer is in String mode
  StringStart
  StringChar(value: UtfCodepoint)
  StringEscape(value: expr.EscapeSequence)
  StringUnicode(value: Int)
  StringInterpolationStart
  StringInterpolationEnd
  StringEnd

  Identifier(value: String)

  /// true
  True
  /// false
  False
  /// public
  Public
  /// internal
  Internal
  /// private
  Private
  /// import
  Import
  /// with
  With
  /// (
  LParen
  /// )
  RParen
  /// [
  LBracket
  /// ]
  RBracket
  /// {
  LBrace
  /// }
  RBrace
  /// type
  Type
  /// struct
  Struct
  /// enum
  Enum
  /// let
  Let
  /// mut
  Mut
  /// external
  External
  /// func
  Func
  /// interface
  Interface
  /// impl
  Impl
  /// namespace
  Namespace
  /// for
  For
  /// while
  While
  /// if
  If
  /// else
  Else
  /// match
  Match
  /// int
  IntType
  /// float
  FloatType
  /// boolean
  BooleanType
  /// char
  CharType
  /// string
  StringType
  /// void
  VoidType
  /// never
  NeverType
  /// #[
  ListStart
  /// #(
  TupleStart
  /// #{
  MapStart
  /// ->
  Arrow
  /// ...
  Spread
  /// ?
  QuestionMark
  /// .
  Dot
  /// @
  AtSign
  /// ,
  Comma
  /// :
  Colon
  /// ::
  DoubleColon
  /// _
  Underscore
  /// |>
  Triangle
  /// ++
  PlusPlus
  /// --
  MinusMinus
  /// !
  ExclamationMark
  /// +
  Plus
  /// \-
  Minus
  /// *
  Star
  /// /
  Slash
  /// %
  Percent
  /// **
  StarStar
  /// &
  And
  /// |
  Pipe
  /// ^
  Caret
  /// <<
  LtLt
  /// >>
  GtGt
  /// ==
  EqEq
  /// !=
  NotEq
  /// <
  Lt
  /// >
  Gt
  /// <=
  LtEq
  /// >=
  GtEq
  /// in
  In
  /// !in
  NotIn
  /// &&
  AndAnd
  /// ||
  PipePipe
  /// as
  As
  /// is
  Is
  /// !is
  NotIs
  /// =
  Eq
  /// +=
  PlusEq
  /// -=
  MinusEq
  /// *=
  StarEq
  /// /=
  SlashEq
  /// %=
  PercentEq
  /// **=
  StarStarEq
  /// &&=
  AndAndEq
  /// ||=
  PipePipeEq
  /// &=
  AndEq
  /// |=
  PipeEq
  /// ^=
  CaretEq
  /// <<=
  LtLtEq
  /// >>=
  GtGtEq
  /// ..
  DotDot
  /// ..=
  DotDotEq
  /// continue
  Continue
  /// break
  Break
  /// return
  Return
  /// this
  This

  Whitespace(content: String)
  Comment(content: String)
  CommentMultiline(content: String)
}

pub fn to_string(token: NymphToken) -> String {
  case token {
    BinaryInt(_) -> "a binary integer literal"
    OctalInt(_) -> "an octal integer literal"
    HexInt(_) -> "a hexadecimal integer literal"
    DecimalInt(_) -> "an integer literal"
    FloatBothParts(_) -> "a floating point literal"
    FloatFirstPart(_) -> "a floating point literal"
    FloatSecondPart(_) -> "a floating point literal"
    FloatInt(_) -> "a floating point literal"
    FloatNoExponent(_) -> "a floating point literal"
    Identifier(_) -> "an identifier"
    Char(_) -> "a character literal"
    CharEscape(_) -> "a character literal"
    CharUnicode(_) -> "a character literal"
    StringStart -> "a string literal"
    StringChar(_) -> "a character"
    StringEscape(_) -> "an escape sequence"
    StringUnicode(_) -> "an escape sequence"
    StringInterpolationStart -> "an interpolated expression"
    StringInterpolationEnd -> "an interpolated expression closer"
    StringEnd -> "the end of a string literal"
    True -> "true"
    False -> "false"
    Public -> "public"
    Internal -> "internal"
    Private -> "private"
    Import -> "import"
    With -> "with"
    LParen -> "("
    RParen -> ")"
    LBracket -> "["
    RBracket -> "]"
    LBrace -> "{"
    RBrace -> "}"
    Type -> "type"
    Struct -> "struct"
    Enum -> "enum"
    Let -> "let"
    Mut -> "mut"
    External -> "external"
    Func -> "func"
    Interface -> "interface"
    Impl -> "impl"
    Namespace -> "namespace"
    For -> "for"
    While -> "while"
    If -> "if"
    Else -> "else"
    Match -> "match"
    IntType -> "int"
    FloatType -> "float"
    BooleanType -> "boolean"
    CharType -> "char"
    StringType -> "string"
    VoidType -> "void"
    NeverType -> "never"
    ListStart -> "#["
    TupleStart -> "#("
    MapStart -> "#{"
    Arrow -> "->"
    Spread -> "..."
    QuestionMark -> "?"
    Dot -> "."
    AtSign -> "@"
    Comma -> ","
    Colon -> ":"
    DoubleColon -> "::"
    Underscore -> "_"
    Triangle -> "|>"
    PlusPlus -> "++"
    MinusMinus -> "--"
    ExclamationMark -> "!"
    Plus -> "+"
    Minus -> "-"
    Star -> "*"
    Slash -> "/"
    Percent -> "%"
    StarStar -> "**"
    And -> "&"
    Pipe -> "|"
    Caret -> "^"
    LtLt -> "<<"
    GtGt -> ">>"
    EqEq -> "=="
    NotEq -> "!="
    Lt -> "<"
    Gt -> ">"
    LtEq -> "<="
    GtEq -> ">="
    In -> "in"
    NotIn -> "!in"
    AndAnd -> "&&"
    PipePipe -> "||"
    As -> "as"
    Is -> "is"
    NotIs -> "!is"
    Eq -> "="
    PlusEq -> "+="
    MinusEq -> "-="
    StarEq -> "*="
    SlashEq -> "/="
    PercentEq -> "%="
    StarStarEq -> "**="
    AndAndEq -> "&&="
    PipePipeEq -> "||="
    AndEq -> "&="
    PipeEq -> "|="
    CaretEq -> "^="
    LtLtEq -> "<<="
    GtGtEq -> ">>="
    DotDot -> ".."
    DotDotEq -> "..="
    Continue -> "continue"
    Break -> "break"
    Return -> "return"
    This -> "this"
    Whitespace(_) -> "whitespace"
    Comment(_) -> "a comment"
    CommentMultiline(_) -> "a multiline comment"
  }
}
