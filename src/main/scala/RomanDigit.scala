sealed trait RomanDigit extends Ordered[RomanDigit] with Product with Serializable {
  def compare(that: RomanDigit) = {
    if (this == that) 0
    else if (this < that) 1
    else -1
  }

  override def <(that: RomanDigit): Boolean = {
    that match {
      case V if this == I => true
      case X if this < V || this == V => true
      case L if this < X || this == X => true
      case C if this < L || this == L => true
      case D if this < C || this == C => true
      case M if this < D || this == D => true
      case _ => false
    }
  }
}

case object I extends RomanDigit
case object V extends RomanDigit
case object X extends RomanDigit
case object L extends RomanDigit
case object C extends RomanDigit
case object D extends RomanDigit
case object M extends RomanDigit
