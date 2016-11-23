sealed trait RomanDigit extends Ordered[RomanDigit] with Product with Serializable {
  def compare(that: RomanDigit) = {
    if (this == that) 0
    else if (this < that) 1
    else -1
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

  override def <=(that: RomanDigit): Boolean = this < that || this == that

  override def >(that: RomanDigit): Boolean = !(this <= that)

  override def >=(that: RomanDigit): Boolean = this > that || this == that
}

case object I extends RomanDigit
case object V extends RomanDigit
case object X extends RomanDigit
case object L extends RomanDigit
case object C extends RomanDigit
case object D extends RomanDigit
case object M extends RomanDigit
