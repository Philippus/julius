sealed trait RomanDigit extends Ordered[RomanDigit] with Product with Serializable {
  import RomanDigit.{I, V, X, L, C, D, M}

  def compare(that: RomanDigit): Int = {
    if (this == that) 0
    else if (this < that) -1
    else 1
  }

  override def <(that: RomanDigit): Boolean = that match {
    case I => false
    case V => this == I
    case X => this <= V
    case L => this <= X
    case C => this <= L
    case D => this <= C
    case M => this <= D
  }
}

object RomanDigit {
  case object I extends RomanDigit
  case object V extends RomanDigit
  case object X extends RomanDigit
  case object L extends RomanDigit
  case object C extends RomanDigit
  case object D extends RomanDigit
  case object M extends RomanDigit

  implicit class RomanDigitOps(lhs: RomanDigit) {
    def +(rhs: RomanDigit): RomanNumeral = RomanNumeral(lhs).plus(RomanNumeral(rhs))
    def -(rhs: RomanDigit): RomanNumeral = RomanNumeral(lhs).minus(RomanNumeral(rhs))
    def *(rhs: RomanDigit): RomanNumeral = RomanNumeral(lhs).times(RomanNumeral(rhs))
    def /(rhs: RomanDigit): RomanNumeral = RomanNumeral(lhs).div(RomanNumeral(rhs))
    def +(rhs: RomanNumeral): RomanNumeral = RomanNumeral(lhs).plus(rhs)
    def -(rhs: RomanNumeral): RomanNumeral = RomanNumeral(lhs).minus(rhs)
    def *(rhs: RomanNumeral): RomanNumeral = RomanNumeral(lhs).times(rhs)
    def /(rhs: RomanNumeral): RomanNumeral = RomanNumeral(lhs).div(rhs)
  }
}
