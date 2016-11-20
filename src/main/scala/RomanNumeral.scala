sealed trait RomanNumeral {
  import RomanNumeral.{Nulla, RomanDigits}
  def +(that: RomanNumeral): RomanNumeral = {
    this match {
      case Nulla => that
      case RomanDigits(l) => that match {
        case Nulla => RomanNumeral(l)
        case RomanDigits(r) => RomanNumeral((l ++ r).sortWith(_ < _))
      }
    }
  }
}

object RomanNumeral {
  case object Nulla extends RomanNumeral
  final case class RomanDigits(l: List[RomanDigit]) extends RomanNumeral

  def apply(): RomanNumeral = Nulla
  def apply(l: List[RomanDigit]): RomanNumeral = {
    if (l.isEmpty) Nulla else RomanDigits(l)
  }
}
