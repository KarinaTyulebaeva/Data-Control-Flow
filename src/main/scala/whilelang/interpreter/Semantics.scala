package whilelang.interpreter

import scala.collection.mutable.Map
import whilelang.parser._
import whilelang.parser.Statement._
import whilelang.parser.Expression._
import whilelang.parser.Bool._
import scala.collection.mutable.Set
import scala.collection.mutable.ListBuffer


type Environment = Map[String, Int]
given Environment = Map[String, Int]()

type Availability = Set[Expression]
var availability: Availability = Set.empty

given ListBuffer[(Node, Boolean)] = ListBuffer.empty

extension (stmt: Statement)(using env: Map[String, Int])
  def execute: List[(Node, Boolean)] = stmt match

    case If(cond, tSmt, eSmt) =>
      (if cond.value then tSmt else eSmt).execute
      List((Condition(cond, tSmt.execute, eSmt.execute), false))

    case Write(exp) =>
      println(exp.value)
      List((DoesNotChangeAvailability("print"), false))

    case While(cond, doSmt) =>
      while cond.value do doSmt.execute
      List((Loop(cond, doSmt.execute), false))

    case Print(text) =>
      println(text)
      List((DoesNotChangeAvailability("print"), false))

    case SeqStatement(stmts) =>
      stmts.flatMap(_.execute)

    case Attrib(id, exp) =>
      env += id -> exp.value
      List((Assignment(id, exp), false))

    case Program(seq) => seq.execute
    case Skip | _ => List((DoesNotChangeAvailability("empty"), false))

  def checkAvailability(program: List[(Node, Boolean)]): List[(Node, Boolean)] =
    program.map { line =>
      line match
        case (Assignment(id, value), _) => (line._1, checkAvailabilityForAssignment(id, value))
        case (Condition(condition, left, right), _) =>
          val (res, newLeft, newRight) = checkAvailabilityForCondition(condition, left, right)
          (Condition(condition, newLeft, newRight), res)
        case (Loop(condition, body), _) =>
          val (newBody, res) = checkAvailabilityForLoop(condition, body)
          (Loop(condition, newBody), res)

        case (DoesNotChangeAvailability(_), _) => line
        case _ => line
    }

  def checkAvailabilityForBool(value: Bool): Boolean =
    value match {
      case ExpEq(l, r) => isAvailable(r)
      case ExpLe(l, r) => isAvailable(r)
      case _ => true
    }

  def checkAvailabilityForCondition(condition: Bool, left: List[(Node, Boolean)], right: List[(Node, Boolean)]): (Boolean, List[(Node, Boolean)], List[(Node, Boolean)]) =
    val res = checkAvailabilityForBool(condition)
    val (set1: Availability, newLeft: List[(Node, Boolean)]) = addOnly(left)
    val (set2: Availability, newRight: List[(Node, Boolean)]) = addOnly(right)
    availability.addAll(set1.intersect(set2))
    (res, newLeft, newRight)

  def checkAvailabilityForAssignment(id: String, assignment: Expression): Boolean = {
    val res = availability.contains(assignment)
    removeAvailability(id)
    addAvailability(assignment)
    res
  }

  def addOnly(elems: List[(Node, Boolean)]): (Availability, List[(Node, Boolean)]) =
    var newAvailability: Availability = Set.empty
    (newAvailability, elems.map { elem =>
      elem match {
        case (Assignment(id, value), _) =>
          val isAvailable = availability.contains(value)
          removeAvailability(id)
          newAvailability.addOne(value)
          (Assignment(id, value), isAvailable)
      }
    })

  def isAvailable(expression: Expression): Boolean = availability.contains(expression)

  def iterationOneForLoop(body: List[(Node, Boolean)]): Unit = body.foreach {
    case (Assignment(id, value), _) => {
      removeAvailability(id)
      addAvailability(value) //спорный момент
    }
  }

  def iterationTwoForLoop(condition: Bool, body: List[(Node, Boolean)]): Boolean = {
    val resForCondition = condition match {
      case ExpEq(left, right) if availability.contains(right) => true
      case _ => false
    }
    checkAvailability(body)
    resForCondition
  }

  def checkAvailabilityForLoop(condition: Bool, body: List[(Node, Boolean)]): (List[(Node, Boolean)], Boolean) =
    val before = checkAvailabilityForBool(condition)
    val newBody = body.map {
      case elem@(Assignment(id, value), _) => (elem._1, checkAvailabilityForAssignment(id, value))
      case elem@(Condition(condition, left, right), _) =>
        val (res, newLeft, newRight) = checkAvailabilityForCondition(condition, left, right)
        (Condition(condition, newLeft, newRight), res)
      case elem@(Loop(cond, elems), _) =>
        val (newElems, res) = checkAvailabilityForLoop(cond, elems)
        (Loop(cond, newElems), res)
      case elem => elem
    }
    val after = checkAvailabilityForBool(condition)
    (newBody, before && after)

  def removeAvailability(id: String): Unit =
    val newAvailability = availability.filter { elem =>
      elem match {
        case Read => false
        case Id(innerId) if innerId == id => false
        case ExpSum(left, right) => (left, right) match {
          case (Id(left), Id(right)) if left == id || right == id => false
          case _ => true
        }
        case ExpSub(left, right) => (left, right) match {
          case (Id(left), Id(right)) if left == id || right == id => false
          case _ => true
        }
        case ExpMult(left, right) => (left, right) match {
          case (Id(left), Id(right)) if left == id || right == id => false
          case _ => true
        }
        case _ => true
      }
    }
    availability = newAvailability


  def addAvailability(value: Expression): Unit = {
    availability.addOne(value)
  }

extension (exp: Expression)(using env: Environment)
  def value: Int = exp match
    case Read => io.StdIn.readInt()
    case Id(id) => env.getOrElseUpdate(id, 0)
    case Integer(value) => value
    case ExpSum(lhs, rhs) => lhs.value + rhs.value
    case ExpSub(lhs, rhs) => lhs.value - rhs.value
    case ExpMult(lhs, rhs) => lhs.value * rhs.value
    case null | _ => 0

extension (exp: Bool)(using env: Environment)
  def value: Boolean = exp match
    case Boole(b) => b
    case ExpEq(lhs, rhs) => lhs.value == rhs.value
    case ExpLe(lhs, rhs) => lhs.value <= rhs.value
    case Not(b) => !b.value
    case And(lhs, rhs) => lhs.value && rhs.value
    case null | _ => true


