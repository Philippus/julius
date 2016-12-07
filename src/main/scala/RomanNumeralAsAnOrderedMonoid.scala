import scalaz._
import scalaz.Ordering.{EQ, GT, LT}
import scalaz.Tags.Multiplication

object RomanNumeralAsAnOrderedMonoid {
  implicit object RomanNumeralOrderedMonoid extends Order[RomanNumeral] with Monoid[RomanNumeral] {
    def order(n: RomanNumeral, m: RomanNumeral): Ordering = {
      if (n.compare(m) == 0)  EQ
      else if (n.compare(m) == -1) LT
      else GT
    }

    def zero: RomanNumeral = RomanNumeral()

    def append(n: RomanNumeral, m: => RomanNumeral): RomanNumeral = n + m
  }

  implicit object RomanNumeralOrderedMonoidMultiply extends Order[RomanNumeral @@ Multiplication] with Monoid[RomanNumeral @@ Multiplication] {
    def order(n: RomanNumeral @@ Multiplication, m: RomanNumeral @@ Multiplication): Ordering =
      Order[RomanNumeral].order(Tag.unwrap(n), Tag.unwrap(m))

    def zero: RomanNumeral @@ Multiplication = Multiplication(RomanNumeral(List(I)))

    def append(n: RomanNumeral @@ Multiplication, m: => RomanNumeral @@ Multiplication): RomanNumeral @@ Multiplication
    = Multiplication(Tag.unwrap(n) * Tag.unwrap(m))
  }
}

case class RomanNumeralAsAnOrderedMonoid(r: RomanNumeral)


