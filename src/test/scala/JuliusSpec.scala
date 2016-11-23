import org.scalacheck.Gen._
import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalacheck.Prop.forAll
import JuliusImplicits._

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

  property("RomanNumeral addition is commutative") = forAll {
    (n: RomanNumeral, m: RomanNumeral) => (n + m) == (m + n)
  }

  property("RomanNumeral addition is associative") = forAll {
    (n: RomanNumeral, m: RomanNumeral, o: RomanNumeral) => (n + m) + o == n + (m + o)
  }

  def checkList(l: List[RomanDigit]): Boolean = {
    if (l.isEmpty) true
    else {
      l match {
        case a :: b :: rest => (b < a || b == a) && checkList(b :: rest)
        case _ => true
      }
    }
  }

  property("RomanNumeral always has its digits sorted from high to low") = forAll {
    n: RomanNumeral => n match {
      case RomanNumeral.Nulla => true
      case RomanNumeral.RomanDigits(l) => checkList(l)
    }
  }

  property("RomanNumeral is optimized after addition") = forAll {
    (n: RomanNumeral, m: RomanNumeral) => {
      val o = n + m
      o match {
        case RomanNumeral.Nulla => true
        case RomanNumeral.RomanDigits(l) =>
          !(l containsSlice List(I, I, I, I, I)) &&
          !(l containsSlice List(V, V)) &&
          !(l containsSlice List(X, X, X, X, X)) &&
          !(l containsSlice List(L, L)) &&
          !(l containsSlice List(C, C, C, C, C)) &&
          !(l containsSlice List(D, D))
      }
    }
  }

  property("RomanDigit can be added to another") = forAll {
    (r: RomanDigit, s: RomanDigit) => {
      val o = r + s
      o match {
        case RomanNumeral.Nulla => false
        case RomanNumeral.RomanDigits(_) => true
      }
    }
  }

  property("RomanDigit can be added to a RomanNumeral") = forAll {
    (r: RomanDigit, n: RomanNumeral) => {
      val o = r + n
      o match {
        case RomanNumeral.Nulla => false
        case RomanNumeral.RomanDigits(_) => true
      }
    }
  }

  property("RomanNumeral can be added to a RomanDigit") = forAll {
    (r: RomanDigit, n: RomanNumeral) => {
      val o = n + r
      o match {
        case RomanNumeral.Nulla => false
        case RomanNumeral.RomanDigits(_) => true
      }
    }
  }

  property("adding RomanNumerals and then converting to int is the same as converting to int and then adding") = forAll {
    (n: RomanNumeral, m: RomanNumeral) => (n + m).toInt == n.toInt + m.toInt
  }
}
