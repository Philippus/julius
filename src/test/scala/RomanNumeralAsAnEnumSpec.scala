import JuliusSpec._
import RomanNumeralAsAnEnum._
import org.scalacheck._

import scalaz.scalacheck.ScalazProperties._

object RomanNumeralAsAnEnumSpec extends Properties("Enum") {
  def checkAll(name: String, props: Properties) {
    for ((name2, prop) <- props.properties) yield {
      property(name + ":" + name2) = prop
    }
  }

  implicit val arbitraryRomanNumeral: Arbitrary[RomanNumeral] = Arbitrary(genRomanDigits)

  checkAll("enum laws", enum.laws[RomanNumeral])
}
