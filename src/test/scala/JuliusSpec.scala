import scala.annotation.tailrec
import scala.collection.immutable.Stream

import org.scalacheck.Gen._
import org.scalacheck._
import org.scalacheck.Prop.forAll
import org.scalacheck.Shrink.shrink

import JuliusImplicits._

object JuliusSpec extends Properties("Julius") {
  def genRomanDigit: Gen[RomanDigit] = oneOf(List(M, D, C, L, X, V, I))

  implicit val arbitraryRomanDigit: Arbitrary[RomanDigit] = Arbitrary(genRomanDigit)

  def genLimitedRomanNumeral: Gen[RomanNumeral] = genRomanNumeral retryUntil (_.toInt < 1000)

  def genRomanNumeral: Gen[RomanNumeral] = Gen.oneOf(genNulla, genRomanDigits)

  def genNulla: Gen[RomanNumeral] = RomanNumeral()

  def genRomanDigits: Gen[RomanNumeral] = for {
    x <- nonEmptyListOf(genRomanDigit)
  } yield RomanNumeral(x)

  def genStringOfRomanDigits: Gen[String] = for {
    x <- listOf(genRomanDigit)
  } yield x.mkString

  implicit val arbitraryRomanNumeral: Arbitrary[RomanNumeral] = Arbitrary(genRomanNumeral)

  implicit val shrinkRomanNumeral: Shrink[RomanNumeral] = Shrink {
    case RomanNumeral.Nulla => Stream.empty
    case RomanNumeral.RomanDigits(l) => shrink(l).map(RomanNumeral.apply)
  }

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

  property("RomanNumeral addition has an identity element") = forAll(genNulla, genRomanDigits) {
    (n: RomanNumeral, m: RomanNumeral) => n + m == m + n && n + m == m
  }

  @tailrec def checkList(l: List[RomanDigit]): Boolean = l match {
    case a :: b :: rest => (a >= b) && checkList(b :: rest)
    case _ :: Nil => true
    case Nil => true
  }

  property("RomanNumeral always has its digits sorted from high to low") = forAll {
    n: RomanNumeral => n match {
      case RomanNumeral.Nulla => true
      case RomanNumeral.RomanDigits(l) => checkList(l)
    }
  }

  property("RomanNumeral is optimized after addition") = forAll {
    (n: RomanNumeral, m: RomanNumeral) => n + m match {
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

  property("RomanNumeral is optimized after creation") = forAll {
    (n: RomanNumeral) => n == n.optimize
  }

  property("RomanDigit can be added to another") = forAll {
    (r: RomanDigit, s: RomanDigit) => r + s match {
      case RomanNumeral.Nulla => false
      case RomanNumeral.RomanDigits(_) => true
    }
  }

  property("RomanDigit can be added to a RomanNumeral") = forAll {
    (n: RomanNumeral, r: RomanDigit) => n + r match {
      case RomanNumeral.Nulla => false
      case RomanNumeral.RomanDigits(_) => true
    }
  }

  property("RomanNumeral can be added to a RomanDigit") = forAll {
    (r: RomanDigit, n: RomanNumeral) => r + n match {
      case RomanNumeral.Nulla => false
      case RomanNumeral.RomanDigits(_) => true
    }
  }

  property("RomanDigit can be multiplied by another") = forAll {
    (r: RomanDigit, s: RomanDigit) => r * s match {
      case RomanNumeral.Nulla => true
      case RomanNumeral.RomanDigits(_) => true
    }
  }

  property("RomanNumeral can be multiplied by a RomanDigit") = forAll {
    (n: RomanNumeral, r: RomanDigit) => n * r match {
      case RomanNumeral.Nulla => true
      case RomanNumeral.RomanDigits(_) => true
    }
  }

  property("RomanDigit can be multiplied by a RomanNumeral") = forAll {
    (r: RomanDigit, n: RomanNumeral) => r * n match {
      case RomanNumeral.Nulla => true
      case RomanNumeral.RomanDigits(_) => true
    }
  }

  property("RomanDigit can be subtracted from another") = forAll {
    (r: RomanDigit, s: RomanDigit) => r - s match {
      case RomanNumeral.Nulla => true
      case RomanNumeral.RomanDigits(_) => true
    }
  }

  property("RomanDigit can be subtracted from a RomanNumeral") = forAll {
    (n: RomanNumeral, r: RomanDigit) => n - r match {
      case RomanNumeral.Nulla => true
      case RomanNumeral.RomanDigits(_) => true
    }
  }

  property("RomanNumeral can be subtracted from a RomanDigit") = forAll {
    (r: RomanDigit, n: RomanNumeral) => r - n match {
      case RomanNumeral.Nulla => true
      case RomanNumeral.RomanDigits(_) => true
    }
  }

  property("RomanDigit can be divided by another") = forAll {
    (r: RomanDigit, s: RomanDigit) => r / s match {
      case RomanNumeral.Nulla => true
      case RomanNumeral.RomanDigits(_) => true
    }
  }

  property("RomanNumeral can be divided by a RomanDigit") = forAll {
    (n: RomanNumeral, r: RomanDigit) => n / r match {
      case RomanNumeral.Nulla => true
      case RomanNumeral.RomanDigits(_) => true
    }
  }

  property("RomanDigit can be divided by a RomanNumeral") = forAll(genRomanDigit, genRomanDigits) {
    (r: RomanDigit, n: RomanNumeral) => r / n match {
      case RomanNumeral.Nulla => true
      case RomanNumeral.RomanDigits(_) => true
    }
  }

  property("RomanNumeral division by Nulla results in ArithmeticException") = forAll {
    (n: RomanNumeral) => Prop.throws(classOf[ArithmeticException]) { n / RomanNumeral.Nulla }
  }

  property("RomanNumeral isOdd") = forAll {
    (n: RomanNumeral) => n.isOdd == (n.toInt % 2 != 0)
  }

  property("RomanNumeral doubling then halving") = forAll {
    (n: RomanNumeral) => n == n.double.halve
  }

  property("RomanNumeral halving then doubling") = forAll {
    (n: RomanNumeral) => {
      if (n.isOdd) n == n.halve.double + I
      else n == n.halve.double
    }
  }

  property("RomanNumeral multiplication is commutative") = forAll {
    (n: RomanNumeral, m: RomanNumeral) => n * m == m * n
  }

  property("RomanNumeral multiplication is associative") = forAll(genLimitedRomanNumeral, genLimitedRomanNumeral, genLimitedRomanNumeral) {
    (n: RomanNumeral, m: RomanNumeral, o: RomanNumeral) => (n * m) * o == n * (m * o)
  }

  property("RomanNumeral multiplication has an identity element") = forAll(genRomanNumeral) {
    n: RomanNumeral => n * I == I * n && n * I == n
  }

  property("RomanNumeral adding is left distributive") = forAll {
    (n: RomanNumeral, m: RomanNumeral, o: RomanNumeral) => n * (m + o) == (n * m) + (n * o)
  }

  property("RomanNumeral adding is right distributive") = forAll {
    (n: RomanNumeral, m: RomanNumeral, o: RomanNumeral) => (m + o) * n == (m * n) + (o * n)
  }

  property("RomanNumeral adding then subtracting") = forAll {
    (n: RomanNumeral, m: RomanNumeral) => n == n + m - m
  }

  property("RomanNumeral subtracting is left distributive") = forAll {
    (n: RomanNumeral, m: RomanNumeral, o: RomanNumeral) => n * (m - o) == (n * m) - (n * o)
  }

  property("RomanNumeral subtracting is right distributive") = forAll {
    (n: RomanNumeral, m: RomanNumeral, o: RomanNumeral) => (m - o) * n == (m * n) - (o * n)
  }

  property("RomanNumeral multiplying then dividing") = forAll {
    (n: RomanNumeral, m: RomanNumeral) => m match {
      case RomanNumeral.Nulla => true
      case RomanNumeral.RomanDigits(_) => n == (n * m) / m
    }
  }

  property("adding RomanNumerals and then converting to int is the same as converting to int and then adding") = forAll {
    (n: RomanNumeral, m: RomanNumeral) => (n + m).toInt == n.toInt + m.toInt
  }

  property("RomanDigit comparison: <") = forAll {
    (r: RomanDigit, s: RomanDigit) => (r < s) == (r.toInt < s.toInt)
  }

  property("RomanDigit comparison: <=") = forAll {
    (r: RomanDigit, s: RomanDigit) => (r <= s) == (r.toInt <= s.toInt)
  }

  property("RomanDigit comparison: >") = forAll {
    (r: RomanDigit, s: RomanDigit) => (r > s) == (r.toInt > s.toInt)
  }

  property("RomanDigit comparison: >=") = forAll {
    (r: RomanDigit, s: RomanDigit) => (r >= s) == (r.toInt >= s.toInt)
  }

  property("RomanNumeral comparison: <") = forAll {
    (n: RomanNumeral, m: RomanNumeral) => (n < m) == (n.toInt < m.toInt)
  }

  property("RomanNumeral comparison: <=") = forAll {
    (n: RomanNumeral, m: RomanNumeral) => (n <= m) == (n.toInt <= m.toInt)
  }

  property("RomanNumeral comparison: >") = forAll {
    (n: RomanNumeral, m: RomanNumeral) => (n > m) == (n.toInt > m.toInt)
  }

  property("RomanNumeral comparison: >=") = forAll {
    (n: RomanNumeral, m: RomanNumeral) => (n >= m) == (n.toInt >= m.toInt)
  }

  property("RomanNumeral to String and back") = forAll {
    (n: RomanNumeral) => n == n.toString.toRomanNumeral.getOrElse(false)
  }

  property("RomanNumeral to Int and back") = forAll {
    (n: RomanNumeral) => n == n.toInt.toRomanNumeral
  }

  property("RomanNumeral created from Roman Digits is validated") = forAll(genStringOfRomanDigits) {
    (s: String) => s.toRomanNumeral.isEmpty || s.toRomanNumeral.nonEmpty
  }

  property("RomanNumeral created from String is validated") = forAll {
    (s: String) => s.toRomanNumeral.isEmpty || s.toRomanNumeral.nonEmpty
  }
}
