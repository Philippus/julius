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
    r: RomanDigit => r == I || r == V || r == X || r == L || r == C || r == D || r == M
  }

  property("RomanNumeral.generator only generates roman numerals") = forAll {
    n: RomanNumeral => n == RomanNumeral.Nulla || n.isInstanceOf[RomanNumeral.RomanDigits]
  }

  property("RomanNumeral should be Nulla or consist of one or more digits") = forAll {
    n: RomanNumeral => n match {
      case RomanNumeral.Nulla => true
      case RomanNumeral.RomanDigits(l) => l.nonEmpty
    }
  }
}
