sealed trait RomanNumeral

object RomanNumeral {
  case object Nulla extends RomanNumeral
  final case class RomanDigits(l: List[RomanDigit]) extends RomanNumeral

  def apply(): RomanNumeral = Nulla
  def apply(l: List[RomanDigit]): RomanNumeral = {
    if (l.isEmpty) Nulla else RomanDigits(l)
  }
}
