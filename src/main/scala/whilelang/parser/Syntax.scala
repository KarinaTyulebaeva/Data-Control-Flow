package whilelang.parser

enum Statement:
  case Skip
  case If(condition: Bool, thenSmt: Statement, elseSmt: Statement)
  case Write(exp: Expression)
  case While(condition: Bool, doSmt: Statement)
  case Print(text: String)
  case SeqStatement(statements: List[Statement])
  case Attrib(id: String, exp: Expression)
  case Program(statements: SeqStatement)

enum Expression:
  case Read
  case Id(id: String)
  case Integer(exp: Int)
  case ExpSum(lhs: Expression, rhs: Expression)
  case ExpSub(lhs: Expression, rhs: Expression)
  case ExpMult(lhs: Expression, rhs: Expression)

enum Bool:
  case Boole(b: Boolean)
  case ExpEq(lhs: Expression, rhs: Expression)
  case ExpLe(lhs: Expression, rhs: Expression)
  case Not(b: Bool)
  case And(lhs: Bool, rhs: Bool)

sealed trait Node

case class Assignment(value: String, assignment: Expression) extends Node

case class Condition(condition: Bool, left: List[(Node, Boolean)], right: List[(Node, Boolean)]) extends Node

case class Loop(condition: Bool, body: List[(Node, Boolean)]) extends Node

case class DoesNotChangeAvailability(name: String) extends Node