import org.scalacheck.Gen._
import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.Prop.forAll

object JuliusSpec extends Properties("Julius") {
  implicit val arbitraryRomanDigit: Arbitrary[RomanDigit] = Arbitrary(
    oneOf(List(M, D, C, L, X, V, I))
  )

  def romanNumerals: Gen[RomanNumeral] = Gen.oneOf(genNulla, genRomanDigits)

  def genNulla: Gen[RomanNumeral] = RomanNumeral()

  def genRomanDigits: Gen[RomanNumeral] = for {
    x <- nonEmptyListOf(oneOf(List(M, D, C, L, X, V, I)))
  } yield RomanNumeral(x)

  property("RomanDigit.generator only generates roman digits") = forAll {
    r: RomanDigit => r == I || r == V || r == X || r == L || r == C || r == D || r == M
  }

  property("RomanNumeral.generator only generates roman numerals") = forAll(romanNumerals) {
    n: RomanNumeral => n == RomanNumeral.Nulla || n.isInstanceOf[RomanNumeral.RomanDigits]
  }
}
