import org.scalacheck.Gen._
import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.Prop.forAll

object JuliusSpec extends Properties("Julius") {
  def genRomanDigit: Gen[RomanDigit] = oneOf(List(M, D, C, L, X, V, I))

  implicit val arbitraryRomanDigit: Arbitrary[RomanDigit] = Arbitrary(genRomanDigit)

  def romanNumerals: Gen[RomanNumeral] = Gen.oneOf(genNulla, genRomanDigits)

  def genNulla: Gen[RomanNumeral] = RomanNumeral()

  def genRomanDigits: Gen[RomanNumeral] = for {
    x <- nonEmptyListOf(genRomanDigit)
  } yield RomanNumeral(x)

  property("RomanDigit.generator only generates roman digits") = forAll {
    r: RomanDigit => r == I || r == V || r == X || r == L || r == C || r == D || r == M
  }

  property("RomanNumeral.generator only generates roman numerals") = forAll(romanNumerals) {
    n: RomanNumeral => n == RomanNumeral.Nulla || n.isInstanceOf[RomanNumeral.RomanDigits]
  }
}
