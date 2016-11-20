import org.scalacheck.Gen._
import org.scalacheck.{Arbitrary, Properties}

object JuliusSpec extends Properties("RomanDigit") {
  implicit val arbitraryRomanDigit: Arbitrary[RomanDigit] = Arbitrary(
    oneOf(List(M, D, C, L, X, V, I))
  )
}
