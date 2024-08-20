pub type PrefixOperator {
  /// !
  Not
  /// \-
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
  /// \-
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
  BitAnd
  /// |
  BitOr
  /// ^
  BitXor
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
  BitAndAssign
  BitXorAssign
  BitOrAssign
  BoolAndAssign
  BoolOrAssign
}
