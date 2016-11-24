import scala.collection.immutable.ListMap

import ExtendedList._

sealed trait RomanNumeral extends Ordered[RomanNumeral] {

  import RomanNumeral.{Nulla, RomanDigits}

  def compare(that: RomanNumeral): Int = {
    if (this == that) 0
    else if (this < that) -1
    else 1
  }

  override def <(that: RomanNumeral): Boolean = this match {
    case Nulla => that != Nulla
    case RomanDigits(l) => that match {
      case Nulla => false
      case RomanDigits(r) =>
        def lessThanHelper(digits: List[RomanDigit], l: List[RomanDigit], r: List[RomanDigit], compare: Int): Int = {
          if (compare == 0 && digits.nonEmpty) lessThanHelper(digits.tail, l, r, l.count(_ == digits.head).compare(r.count(_ == digits.head)))
          else compare
        }
        lessThanHelper(List(M, D, C, L, X, V, I), l, r, 0) < 0
    }
  }

  def +(that: RomanNumeral): RomanNumeral = this match {
    case Nulla => that
    case RomanDigits(l) => that match {
      case Nulla => RomanNumeral(l)
      case RomanDigits(r) => RomanNumeral(l ++ r)
    }
  }

  def -(that: RomanNumeral): RomanNumeral = {
    this match {
      case Nulla => Nulla
      case RomanDigits(l) => that match {
        case Nulla => this
        case RomanDigits(_) if this <= that => Nulla
        case RomanDigits(r) =>
          def minusHelper(r: List[RomanDigit], s: List[RomanDigit]): List[RomanDigit] = {
            val substitutes = ListMap[RomanDigit, List[RomanDigit]](
              V -> List(I, I, I, I, I),
              X -> List(V, V),
              L -> List(X, X, X, X, X),
              C -> List(L, L),
              D -> List(C, C, C, C, C),
              M -> List(D, D))

            def findAndExpand(r: List[RomanDigit], largestRemaining: RomanDigit): List[RomanDigit] = {
              val (largerDigits, smallerOrEqualDigits) = r.partition(_ > largestRemaining)
              val smallestLargerDigit = largerDigits.reverse.head
              largerDigits.init ::: substitutes.getOrElse(smallestLargerDigit, List(smallestLargerDigit)) ::: smallerOrEqualDigits
            }

            if (s.isEmpty) r
            else {
              val rExpanded = findAndExpand(r, s.head)
              minusHelper(rExpanded.diff(s), s.diff(rExpanded))
            }
          }
          RomanNumeral(minusHelper(l.diff(r), r.diff(l)))
      }
    }
  }

  def *(that: RomanNumeral) = this match {
    case Nulla => Nulla
    case RomanDigits(_) => that match {
      case Nulla => Nulla
      case RomanDigits(_) =>
        def multiplyHelper(r: RomanNumeral, s: RomanNumeral, acc: List[(RomanNumeral, RomanNumeral)]): List[(RomanNumeral, RomanNumeral)] = {
          if (r == RomanNumeral(List(I))) acc ::: List((r, s))
          else multiplyHelper(r.halve, s.double, acc ::: List((r, s)))
        }
        multiplyHelper(this, that, acc = List()).filter(_._1.isOdd).map(_._2).reduce(_ + _)
    }
  }

  def halve: RomanNumeral = this match {
    case Nulla => Nulla
    case RomanDigits(l) =>
      def halveHelper(l: List[RomanDigit], acc: List[RomanDigit]): List[RomanDigit] = {
        if (l.isEmpty) acc
        else {
          l match {
            case M :: tl => halveHelper(tl, acc ::: List(D))
            case D :: tl => halveHelper(C :: tl, acc ::: List(C, C))
            case C :: tl => halveHelper(tl, acc ::: List(L))
            case L :: tl => halveHelper(X :: tl, acc ::: List(X, X))
            case X :: tl => halveHelper(tl, acc ::: List(V))
            case V :: tl => halveHelper(I :: tl, acc ::: List(I, I))
            case I :: I :: tl => halveHelper(tl, acc ::: List(I))
            case I :: tl => halveHelper(tl, acc)
            case _ => acc
          }
        }
      }
      RomanNumeral(halveHelper(l, acc = List()))
  }

  def double: RomanNumeral = {
    this + this
  }

  def isOdd: Boolean = this match {
    case Nulla => false
    case RomanDigits(l) =>
      def isOddHelper(l: List[RomanDigit], isOdd: Boolean): Boolean = {
        if (l.isEmpty) isOdd
        else {
          l match {
            case V :: tl => isOddHelper(tl, !isOdd)
            case I :: tl => isOddHelper(tl, !isOdd)
            case _ => isOddHelper(l.tail, isOdd)
          }
        }
      }
      isOddHelper(l, isOdd = false)
  }

  def /(that: RomanNumeral): RomanNumeral = this match {
    case Nulla => Nulla
    case RomanDigits(_) => {
      val numerals = List(M, D, C, L, X, V, I)
      val multiples = numerals.zip(numerals.map(x => RomanNumeral(List(x)) * that))

      def divideHelper(multiples: List[(RomanDigit, RomanNumeral)], result: List[RomanDigit], remainder: RomanNumeral): RomanNumeral = {
        if (multiples.isEmpty) RomanNumeral(result)
        else {
          val multiple = multiples.head
          if (multiple._2 <= remainder) divideHelper(multiples, result ++ List(multiple._1), remainder - multiple._2)
          else divideHelper(multiples.tail, result, remainder)
        }
      }

      divideHelper(multiples, result = List(), remainder = this)
    }
  }

  def optimize: RomanNumeral = this match {
    case Nulla => this
    case RomanDigits(l) =>
      val substitutes = ListMap[List[RomanDigit], List[RomanDigit]](
        List(I, I, I, I, I) -> List(V),
        List(V, V) -> List(X),
        List(X, X, X, X, X) -> List(L),
        List(L, L) -> List(C),
        List(C, C, C, C, C) -> List(D),
        List(D, D) -> List(M)
      )
      var optimizedList = l
      for ((trg, rpl) <- substitutes) {
        optimizedList = optimizedList.replaceSlice(trg, rpl)
      }
      RomanDigits(optimizedList)
  }
}

object RomanNumeral {
  case object Nulla extends RomanNumeral
  final case class RomanDigits(l: List[RomanDigit]) extends RomanNumeral

  def apply(): RomanNumeral = Nulla
  def apply(l: List[RomanDigit]): RomanNumeral = {
    if (l.isEmpty) Nulla else RomanDigits(l.sorted.reverse).optimize
  }
}
