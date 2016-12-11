import scalaz._
import scalaz.Ordering.{EQ, GT, LT}

object RomanNumeralAsAnEnum {
  implicit object RomanNumeralEnum extends Enum[RomanNumeral] {
    def order(n: RomanNumeral, m: RomanNumeral): Ordering = {
      if (n.compare(m) == 0)  EQ
      else if (n.compare(m) == -1) LT
      else GT
    }

    def pred(n: RomanNumeral): RomanNumeral = n - RomanNumeral(List(I))
    def succ(n: RomanNumeral): RomanNumeral = n + RomanNumeral(List(I))
  }

}

case class RomanNumeralAsAnEnum(r: RomanNumeral)
