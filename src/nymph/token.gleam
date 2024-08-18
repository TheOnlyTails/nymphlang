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

  StringStart
  // String tokens, only matched when the lexer is in String mode
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
  /// _
  Underscore
  /// ++
  PlusPlus
  /// --
  MinusMinus
  /// !
  ExclamationMark
  /// +
  Plus
  /// -
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
