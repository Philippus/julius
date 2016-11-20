import org.scalacheck.Gen._
import org.scalacheck.{Arbitrary, Properties}
import org.scalacheck.Prop.forAll

object JuliusSpec extends Properties("RomanDigit") {
  implicit val arbitraryRomanDigit: Arbitrary[RomanDigit] = Arbitrary(
    oneOf(List(M, D, C, L, X, V, I))
  )

  property("generator only generates roman digits") = forAll {
    r: RomanDigit => r == I || r == V || r == X || r == L || r == C || r == D || r == M
  }
}
