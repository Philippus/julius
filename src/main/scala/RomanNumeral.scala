import scala.collection.immutable.ListMap

import ExtendedList._

sealed trait RomanNumeral {
  import RomanNumeral.{Nulla, RomanDigits}
  def +(that: RomanNumeral): RomanNumeral = {
    this match {
      case Nulla => that.optimize
      case RomanDigits(l) => that match {
        case Nulla => RomanNumeral(l).optimize
        case RomanDigits(r) => RomanNumeral(l ++ r).optimize
      }
    }
  }

  def optimize: RomanNumeral = {
    this match {
      case Nulla => this
      case RomanDigits(l) => {
        val substitutes = ListMap[List[RomanDigit], List[RomanDigit]](
          List(I, I, I, I, I) -> List(V),
          List(V, V) -> List(X),
          List(X, X, X, X, X) -> List(L),
          List(L, L) -> List(C),
          List(C, C, C, C, C) -> List(D),
          List(D, D) -> List(M)
        )
        def optimizeHelper(substitutes: ListMap[List[RomanDigit], List[RomanDigit]], l: List[RomanDigit]): List[RomanDigit] = {
          if (substitutes.isEmpty) {
            l
          } else {
            optimizeHelper(substitutes.tail, l.replaceSlice(substitutes.head._1, substitutes.head._2))
          }
        }
        var optimizedList = l
        for ((trg, rpl) <- substitutes) {
          optimizedList = optimizedList.replaceSlice(trg, rpl)
        }
        RomanDigits(optimizedList)
      }
    }
  }
}

object RomanNumeral {
  case object Nulla extends RomanNumeral
  final case class RomanDigits(l: List[RomanDigit]) extends RomanNumeral

  def apply(): RomanNumeral = Nulla
  def apply(l: List[RomanDigit]): RomanNumeral = {
    if (l.isEmpty) Nulla else RomanDigits(l.sortWith(_ < _).reverse)
  }
}
