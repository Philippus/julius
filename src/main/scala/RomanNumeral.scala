import scala.annotation.tailrec
import scala.collection.immutable.ListMap

import ExtendedList._

sealed trait RomanNumeral extends Ordered[RomanNumeral] {

  import RomanNumeral.{Nulla, RomanDigits}

  override def toString: String = this match {
    case Nulla => "nulla"
    case RomanDigits(l) =>
      val substitutes = ListMap[List[RomanDigit], List[RomanDigit]](
        List(D, C, C, C, C) -> List(C, M),
        List(C, C, C, C) -> List(C, D),
        List(L, X, X, X, X) -> List(X, C),
        List(X, X, X, X) -> List(X, L),
        List(V, I, I, I, I) -> List(I, X),
        List(I, I, I, I) -> List(I, V)
      )
      l.substitute(substitutes).mkString
  }

  def compare(that: RomanNumeral): Int = {
    if (this == that) 0
    else if (this < that) -1
    else 1
  }

  override def <(that: RomanNumeral): Boolean = (this, that) match {
    case (Nulla, _) => that != Nulla
    case (_, Nulla) => false
    case (RomanDigits(l), RomanDigits(r)) =>
      @tailrec def lessThanHelper(digits: List[RomanDigit], l: List[RomanDigit], r: List[RomanDigit], compare: Int): Boolean = {
        if (compare != 0 || digits.isEmpty) compare < 0
        else lessThanHelper(digits.tail, l, r, l.count(_ == digits.head).compare(r.count(_ == digits.head)))
      }
      lessThanHelper(List(M, D, C, L, X, V, I), l, r, 0)
  }

  def +(that: RomanNumeral): RomanNumeral = (this, that) match {
    case (Nulla, _) => that
    case (_, Nulla) => this
    case (RomanDigits(l), RomanDigits(r)) => RomanNumeral(l ++ r)
  }

  def -(that: RomanNumeral): RomanNumeral = (this, that) match {
    case (Nulla, _) => Nulla
    case (_, Nulla) => this
    case (RomanDigits(_), RomanDigits(_)) if this <= that => Nulla
    case (RomanDigits(l), RomanDigits(r)) =>
      @tailrec def minusHelper(r: List[RomanDigit], s: List[RomanDigit]): RomanNumeral = {
        val substitutes = ListMap[RomanDigit, List[RomanDigit]](
          V -> List(I, I, I, I, I),
          X -> List(V, V),
          L -> List(X, X, X, X, X),
          C -> List(L, L),
          D -> List(C, C, C, C, C),
          M -> List(D, D))

        def findAndExpand(r: List[RomanDigit], largestRemaining: RomanDigit): List[RomanDigit] = {
          val (largerDigits, smallerOrEqualDigits) = r.partition(_ > largestRemaining)
          val smallestLargerDigit = largerDigits.last
          largerDigits.init ::: substitutes.getOrElse(smallestLargerDigit, List(smallestLargerDigit)) ::: smallerOrEqualDigits
        }

        if (s.isEmpty) RomanNumeral(r)
        else {
          val rExpanded = findAndExpand(r, s.head)
          minusHelper(rExpanded.diff(s), s.diff(rExpanded))
        }
      }
      minusHelper(l.diff(r), r.diff(l))
  }

  def *(that: RomanNumeral): RomanNumeral = (this, that) match {
    case (Nulla, _) => Nulla
    case (_, Nulla) => Nulla
    case (RomanDigits(_), RomanDigits(_)) =>
      @tailrec def multiplyHelper(r: RomanNumeral, s: RomanNumeral, acc: List[(RomanNumeral, RomanNumeral)]): List[(RomanNumeral, RomanNumeral)] = {
        if (r == RomanNumeral(I)) acc ::: List((r, s))
        else multiplyHelper(r.halve, s.double, acc ::: List((r, s)))
      }
      multiplyHelper(this, that, acc = List())
        .filter { case (halve, _) => halve.isOdd }
        .map { case (_, double) => double }
        .reduce(_ + _)
  }

  def halve: RomanNumeral = this match {
    case Nulla => Nulla
    case RomanDigits(l) =>
      @tailrec def halveHelper(l: List[RomanDigit], acc: List[RomanDigit]): RomanNumeral = l match {
        case M :: tl => halveHelper(tl, D :: acc)
        case D :: tl => halveHelper(C :: tl, C :: C :: acc)
        case C :: tl => halveHelper(tl, L :: acc)
        case L :: tl => halveHelper(X :: tl, X :: X :: acc)
        case X :: tl => halveHelper(tl, V :: acc)
        case V :: tl => halveHelper(I :: tl, I :: I :: acc)
        case I :: I :: tl => halveHelper(tl, I :: acc)
        case I :: tl => halveHelper(tl, acc)
        case Nil => RomanNumeral(acc)
      }
      halveHelper(l, acc = List())
  }

  def double: RomanNumeral = this + this

  def isOdd: Boolean = this match {
    case Nulla => false
    case RomanDigits(l) =>
      @tailrec def isOddHelper(l: List[RomanDigit], isOdd: Boolean): Boolean = l match {
        case M :: tl => isOddHelper(tl, isOdd)
        case D :: tl => isOddHelper(tl, isOdd)
        case C :: tl => isOddHelper(tl, isOdd)
        case L :: tl => isOddHelper(tl, isOdd)
        case X :: tl => isOddHelper(tl, isOdd)
        case V :: tl => isOddHelper(tl, !isOdd)
        case I :: tl => isOddHelper(tl, !isOdd)
        case Nil => isOdd
      }
      isOddHelper(l, isOdd = false)
  }

  def /(that: RomanNumeral): RomanNumeral = (this, that) match {
    case (_, Nulla) => throw new ArithmeticException("/ by nulla")
    case (Nulla, _) => Nulla
    case (RomanDigits(_), RomanDigits(_)) =>
      val digits = List(M, D, C, L, X, V, I)
      val multiplicationTable = digits.zip(digits.map(x => RomanNumeral(x) * that))

      @tailrec def divideHelper(multiplicationTable: List[(RomanDigit, RomanNumeral)], acc: List[RomanDigit], remainder: RomanNumeral): RomanNumeral = {
        if (multiplicationTable.isEmpty) RomanNumeral(acc)
        else {
          val (digit, multiple) = multiplicationTable.head
          if (multiple <= remainder) divideHelper(multiplicationTable, acc ++ List(digit), remainder - multiple)
          else divideHelper(multiplicationTable.tail, acc, remainder)
        }
      }
      divideHelper(multiplicationTable, acc = List(), remainder = this)
  }

  def optimize: RomanNumeral = this match {
    case Nulla => Nulla
    case RomanDigits(l) =>
      val substitutes = ListMap[List[RomanDigit], List[RomanDigit]](
        List(I, I, I, I, I) -> List(V),
        List(V, V) -> List(X),
        List(X, X, X, X, X) -> List(L),
        List(L, L) -> List(C),
        List(C, C, C, C, C) -> List(D),
        List(D, D) -> List(M)
      )
      RomanDigits(l.substitute(substitutes))
  }
}

object RomanNumeral {
  case object Nulla extends RomanNumeral
  final case class RomanDigits(l: List[RomanDigit]) extends RomanNumeral

  def apply(): RomanNumeral = RomanNumeral(List())
  def apply(r: RomanDigit): RomanNumeral = RomanNumeral(List(r))
  def apply(l: List[RomanDigit]): RomanNumeral = {
    if (l.isEmpty) Nulla
    else RomanDigits(l.sorted.reverse).optimize
  }
}
