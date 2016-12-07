import scalaz.scalacheck.ScalazProperties._

import org.scalacheck._
import RomanNumeralAsAnOrderedMonoid._
import JuliusSpec._

object RomanNumeralAsAnOrderedMonoidSpec extends Properties("Ordered monoid under addition") {
  def checkAll(name: String, props: Properties) {
    for ((name2, prop) <- props.properties) yield {
      property(name + ":" + name2) = prop
    }
  }

  checkAll("equal laws", equal.laws[RomanNumeral])
  checkAll("order laws", order.laws[RomanNumeral])
  checkAll("monoid laws", monoid.laws[RomanNumeral])
}
