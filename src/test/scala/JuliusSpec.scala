import org.scalacheck.Gen._
import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.Prop.forAll

object JuliusSpec extends Properties("Julius") {
  def genRomanDigit: Gen[RomanDigit] = oneOf(List(M, D, C, L, X, V, I))

  implicit val arbitraryRomanDigit: Arbitrary[RomanDigit] = Arbitrary(genRomanDigit)

  def genRomanNumeral: Gen[RomanNumeral] = Gen.oneOf(genNulla, genRomanDigits)

  def genNulla: Gen[RomanNumeral] = RomanNumeral()

  def genRomanDigits: Gen[RomanNumeral] = for {
    x <- listOf(genRomanDigit)
  } yield RomanNumeral(x)

  implicit val arbitraryRomanNumeral: Arbitrary[RomanNumeral] = Arbitrary(genRomanNumeral)

  property("RomanDigit.generator only generates roman digits") = forAll {
    r: RomanDigit => r match {
      case I => true
      case V => true
      case X => true
      case L => true
      case C => true
      case D => true
      case M => true
    }
  }

  property("RomanNumeral.generator only generates roman numerals") = forAll {
    n: RomanNumeral => n match {
      case RomanNumeral.Nulla => true
      case RomanNumeral.RomanDigits(_) => true
    }
  }

  property("RomanNumeral should be Nulla or consist of one or more digits") = forAll {
    n: RomanNumeral => n match {
      case RomanNumeral.Nulla => true
      case RomanNumeral.RomanDigits(l) => l.nonEmpty
    }
  }
}
