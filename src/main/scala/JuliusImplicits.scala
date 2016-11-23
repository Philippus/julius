object JuliusImplicits {
  implicit class RomanDigitToInt(r: RomanDigit) {
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
}
