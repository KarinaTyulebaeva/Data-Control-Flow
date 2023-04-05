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
    val a = program.map { line =>
      line match
        case (Assignment(id, value), _) =>
          val aboba = availability
          val a = (line._1, checkAvailabilityForAssignment(id, value))
          println(s"$a $aboba")
          a
        case (Condition(condition, left, right), _) =>
          val aboba = availability
          val (res, newLeft, newRight) = checkAvailabilityForCondition(condition, left, right)

          val a = (Condition(condition, newLeft, newRight), res)
          println(s"$a $aboba")
          a

        case (Loop(condition, body), _) =>
          val aboba = availability
          val (newBody, res) = checkAvailabilityForLoop(condition, body)

          val a = (Loop(condition, newBody), res)
          println(s"$a $availability")
          a

        case (DoesNotChangeAvailability(_), _) =>
          println(s"$line $availability")
          line
        case _ =>
          println(s"$line $availability")
          line
    }
    a

  def checkAvailabilityForBool(value: Bool): Boolean =
    value match {
      case ExpEq(l, r) =>
        val a = isAvailable(r)
        availability.addOne(r)
        a
      case ExpLe(l, r) =>
        val a = isAvailable(r)
        availability.addOne(r)
        a
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
    val aBefore = availability
    val before = checkAvailabilityForBool(condition)
    println(s"$condition $aBefore")
    val newBody = List(body.map {
      case elem@(Assignment(id, value), _) =>
        val aboba = availability
        val a = (elem._1, checkAvailabilityForAssignment(id, value))
        println(s"$a $aboba")
        a
      case elem@(Condition(condition, left, right), _) =>
        val aboba = availability
        val (res, newLeft, newRight) = checkAvailabilityForCondition(condition, left, right)
        val a =(Condition(condition, newLeft, newRight), res)
        println(s"$a $aboba")
        a
      case elem@(Loop(cond, elems), _) =>
        val aboba = availability
        val (newElems, res) = checkAvailabilityForLoop(cond, elems)
        val a = (Loop(cond, newElems), res)
        println(s"$a $aboba")
        a
      case elem =>
        println(s"$elem $availability")
        elem
    }).map(recursivelyCheckLoop(condition, _)).head
    val after = condition match {
      case ExpEq(l, r) =>
        val a = isAvailable(r)
        a
      case ExpLe(l, r) =>
        val a = isAvailable(r)
        a
      case _ => true
    }
    (newBody, before && after)

  def recursivelyCheckLoop(condition: Bool, body: List[(Node, Boolean)]): List[(Node, Boolean)] =
    val aBefore = availability
    val before = checkAvailabilityForBool(condition)
    println(s"$condition $availability")
    val a = body.map {
      case elem@(Assignment(id, value), _) =>
        val aboba = availability
        val a = (elem._1, checkAvailabilityForAssignment(id, value))
        println(s"$a $aboba")
        a

      case elem@(Condition(condition, left, right), _) =>
        val aboba = availability
        val (res, newLeft, newRight) = checkAvailabilityForCondition(condition, left, right)

        val a = (Condition(condition, newLeft, newRight), res)
        println(s"$a $aboba")
        a
      case elem@(Loop(cond, elems), _) =>
        val aboba = availability
        val (newElems, res) = checkAvailabilityForLoop(cond, elems)
        val a =  (Loop(cond, newElems), res)
        println(s"$a $aboba")
        a
      case elem =>
        println(s"$elem $availability")
        elem
    }
    a

  def removeAvailability(id: String): Unit =
    val newAvailability = availability.filter { elem =>
      elem match {
        case Read => false
        case Id(innerId) if innerId == id => false
        case ExpSum(left, right) => (left, right) match {
          case (Id(left), Id(right)) if left == id || right == id => false
          case (Id(left), ExpSub(leftI: Id, rightI: Id)) if leftI.id == id || rightI.id == id => false
          case (Id(left), ExpSum(leftI: Id, rightI: Id)) if leftI.id == id || rightI.id == id => false
          case (Id(left), ExpSub(leftI: Id, rightI: Integer)) if leftI.id == id  => false
          case (Id(left), ExpSum(leftI: Id, rightI: Integer)) if leftI.id == id  => false
          case _ => true
        }
        case ExpSub(left, right) => (left, right) match {
          case (Id(left), Id(right)) if left == id || right == id =>
            false
          case (Id(left), ExpSub(leftI: Id, rightI: Id)) if leftI.id == id || rightI.id == id => false
          case (Id(left), ExpSum(leftI: Id, rightI: Id)) if leftI.id == id || rightI.id == id => false
          case (Id(left), ExpSub(leftI: Id, rightI: Integer)) if leftI.id == id  => false
          case (Id(left), ExpSum(leftI: Id, rightI: Integer)) if leftI.id == id  => false
          case (Id(left), Integer(aboba)) if left == id  =>
            false
          case _ => true
        }
        case ExpMult(left, right) => (left, right) match {
          case (Id(left), Id(right)) if left == id || right == id => false
          case (Id(left), ExpSub(leftI: Id, rightI: Id)) if leftI.id == id || rightI.id == id => false
          case (Id(left), ExpSum(leftI: Id, rightI: Id)) if leftI.id == id || rightI.id == id => false
          case (Id(left), ExpSub(leftI: Id, rightI: Integer)) if leftI.id == id  => false
          case (Id(left), ExpSum(leftI: Id, rightI: Integer)) if leftI.id == id  => false
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


