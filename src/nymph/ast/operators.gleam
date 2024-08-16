pub type PrefixOperator {
  /// !
  Not
  /// -
  Negate
}

pub type PostfixOperator {
  /// ++
  Increment
  /// --
  Decrement
}

pub type BinaryOperator {
  /// +
  Plus
  /// -
  Minus
  /// *
  Times
  /// /
  Divide
  /// %
  Modulus
  /// **
  Power
  /// &
  BinAnd
  /// |
  BinOr
  /// ^
  BinXor
  /// <<
  LeftShift
  /// >>
  RightShift
  /// ==
  Equals
  /// !=
  NotEquals
  /// <
  LessThan
  /// <=
  LessThanEquals
  /// >
  GreaterThan
  /// >=
  GreaterThanEquals
  /// in
  In
  /// !in
  NotIn
  /// &&
  BoolAnd
  /// ||
  BoolOr
  /// |>
  Pipe
}

pub type TypeOperator {
  // as
  As
  // is
  Is
  // !is
  NotIs
}

pub type AssignOperator {
  Assign
  PlusAssign
  MinusAssign
  TimesAssign
  DivideAssign
  ModulusAssign
  PowerAssign
  LeftShiftAssign
  RightShiftAssign
  BinAndAssign
  BinXorAssign
  BinOrAssign
  BoolAndAssign
  BoolOrAssign
}