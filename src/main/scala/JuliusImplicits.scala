import scala.collection.immutable.ListMap
import scala.language.implicitConversions

import ExtendedList._

object JuliusImplicits {
  implicit class RomanDigitToInt(r: RomanDigit) {
    def +(that: RomanDigit): RomanNumeral = RomanNumeral(List(r, that))

    def +(that: RomanNumeral): RomanNumeral = RomanNumeral(List(r)) + that

    def toInt: Int = r match {
      case I => 1
      case V => 5
      case X => 10
      case L => 50
      case C => 100
      case D => 500
      case M => 1000
    }
  }

  implicit class RomanNumeralToInt(n: RomanNumeral) {
    def +(that: RomanDigit): RomanNumeral = n + RomanNumeral(List(that))

    def toInt: Int = n match {
      case RomanNumeral.Nulla => 0
      case RomanNumeral.RomanDigits(l) => l.map(_.toInt).sum
    }
  }

  implicit class IntToRomanNumeral(i: Int) {
    def toRomanNumeral: RomanNumeral = {
      def intToRomanNumeralHelper(digits: List[RomanDigit], acc: List[RomanDigit], remainder: Int): RomanNumeral = {
        if (digits.isEmpty || remainder == 0) RomanNumeral(acc)
        else {
          val digit = digits.head
          if (digit.toInt <= remainder) {
            intToRomanNumeralHelper(digits, acc ++ List(digit), remainder - digit.toInt)
          } else {
            intToRomanNumeralHelper(digits.tail, acc, remainder)
          }
        }
      }
      intToRomanNumeralHelper(digits = List(M, D, C, L, X, V, I), acc = List(), remainder = i)
    }
  }

  implicit class StringToRomanNumeral(s: String) {
    implicit def charToRomanDigit(c: Char): Option[RomanDigit] = c match {
      case 'I' => Option(I)
      case 'V' => Option(V)
      case 'X' => Option(X)
      case 'L' => Option(L)
      case 'C' => Option(C)
      case 'D' => Option(D)
      case 'M' => Option(M)
      case _ => None
    }

    def uncompact(l: List[RomanDigit]): List[RomanDigit] = {
      val substitutes = ListMap[List[RomanDigit], List[RomanDigit]](
        List(I, V) -> List(I, I, I, I),
        List(I, X) -> List(V, I, I, I, I),
        List(X, L) -> List(X, X, X, X),
        List(X, C) -> List(L, X, X, X, X),
        List(C, D) -> List(C, C, C, C),
        List(C, M) -> List(D, C, C, C, C))
      var uncompactedList = l
      for ((trg, rpl) <- substitutes) {
        uncompactedList = uncompactedList.replaceSlice(trg, rpl)
      }
      uncompactedList
    }

    def validMs(l: List[RomanDigit]): Boolean = l match {
      case M :: tl => validMs(tl)
      case _ => validCs(l)
    }

    def validCs(l: List[RomanDigit]): Boolean = l match {
      case C :: M :: tl => validXs(tl)
      case D :: C :: C :: C :: tl => validXs(tl)
      case D :: C :: C :: tl => validXs(tl)
      case D :: C :: tl => validXs(tl)
      case D :: tl => validXs(tl)
      case C :: D :: tl => validXs(tl)
      case C :: C :: C :: tl => validXs(tl)
      case C :: C :: tl => validXs(tl)
      case C :: tl => validXs(tl)
      case _ => validXs(l)
    }

    def validXs(l: List[RomanDigit]): Boolean = l match {
      case X :: C :: tl => validIs(tl)
      case L :: X :: X :: X :: tl => validIs(tl)
      case L :: X :: X :: tl => validIs(tl)
      case L :: X :: tl => validIs(tl)
      case L :: tl => validIs(tl)
      case X :: L :: tl => validIs(tl)
      case X :: X :: X :: tl => validIs(tl)
      case X :: X :: tl => validIs(tl)
      case X :: tl => validIs(tl)
      case _ => validIs(l)
    }

    def validIs(l: List[RomanDigit]): Boolean = l match {
      case I :: X :: Nil => true
      case V :: I :: I :: I :: Nil => true
      case V :: I :: I :: Nil => true
      case V :: I :: Nil => true
      case V :: Nil => true
      case I :: V :: Nil => true
      case I :: I :: I :: Nil => true
      case I :: I :: Nil => true
      case I :: Nil => true
      case Nil => true
      case _ => false
    }
    def toRomanNumeral: Option[RomanNumeral] = {
      if (s == "nulla") Option(RomanNumeral())
      else {
        val candidate = s.map(charToRomanDigit).toList
        if (candidate.contains(None)) None
        else {
          if (validMs(candidate.flatten))
            Option(RomanNumeral(uncompact(candidate.flatten)))
          else None
        }
      }
    }
  }
}
