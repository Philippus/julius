import scalaz.scalacheck.ScalazProperties._
import org.scalacheck.{Arbitrary, _}
import RomanNumeralAsAnOrderedMonoid._
import JuliusSpec._

import scalaz.Tags.Multiplication
import scalaz.{@@, Tag, Tags}

object RomanNumeralMultMonoidSpec extends Properties("Ordered monoid under multiplication") {
  def checkAll(name: String, props: Properties) {
    for ((name2, prop) <- props.properties) yield {
      property(name + ":" + name2) = prop
    }
  }
  def arb[A: Arbitrary]: Arbitrary[A] = implicitly[Arbitrary[A]]
  implicit val RomanNumeralMultiplicationArbitrary: Arbitrary[RomanNumeral @@ Multiplication] = Tag.subst(arb[RomanNumeral])

  checkAll("equal laws", equal.laws[RomanNumeral @@ Tags.Multiplication])
  checkAll("order laws", order.laws[RomanNumeral @@ Tags.Multiplication])
  checkAll("monoid laws", monoid.laws[RomanNumeral @@ Tags.Multiplication])
}
