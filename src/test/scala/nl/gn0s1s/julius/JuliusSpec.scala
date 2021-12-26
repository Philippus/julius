package nl.gn0s1s.julius

import scala.annotation.tailrec

import org.scalacheck._
import org.scalacheck.Prop.forAll

object JuliusSpec extends Properties("Julius") {
  import Generators._
  import RomanDigit._
  import RomanNumeral._

  property("RomanDigit.generator only generates roman digits") = forAll { r: RomanDigit =>
    r match {
      case I => true
      case V => true
      case X => true
      case L => true
      case C => true
      case D => true
      case M => true
    }
  }

  property("RomanNumeral.generator only generates roman numerals") = forAll { n: RomanNumeral =>
    n match {
      case RomanNumeral.Nulla => true
      case RomanNumeral.RomanDigits(_) => true
    }
  }

  property("RomanNumeral should be Nulla or consist of one or more digits") = forAll { n: RomanNumeral =>
    n match {
      case RomanNumeral.Nulla => true
      case RomanNumeral.RomanDigits(l) => l.nonEmpty
    }
  }

  property("RomanNumeral addition is commutative") = forAll { (n: RomanNumeral, m: RomanNumeral) =>
    n.plus(m) == m.plus(n)
  }

  property("RomanNumeral addition is associative") = forAll { (n: RomanNumeral, m: RomanNumeral, o: RomanNumeral) =>
    n.plus(m).plus(o) == n.plus(m.plus(o))
  }

  property("RomanNumeral addition has an identity element") = forAll(genNulla, genRomanDigits) {
    (n: RomanNumeral, m: RomanNumeral) => n.plus(m) == m.plus(n) && m == n.plus(m)
  }

  @tailrec def checkList(l: List[RomanDigit]): Boolean = l match {
    case a :: b :: rest => (a >= b) && checkList(b :: rest)
    case _ => true
  }

  property("RomanNumeral always has its digits sorted from high to low") = forAll { n: RomanNumeral =>
    n match {
      case RomanNumeral.Nulla => true
      case RomanNumeral.RomanDigits(l) => checkList(l)
    }
  }

  property("RomanNumeral is optimized after addition") = forAll { (n: RomanNumeral, m: RomanNumeral) =>
    n.plus(m) match {
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

  property("RomanNumeral is optimized after creation") = forAll { (n: RomanNumeral) =>
    n == n.optimize
  }

  property("RomanDigit can be added to another") = forAll { (r: RomanDigit, s: RomanDigit) =>
    r + s match {
      case RomanNumeral.Nulla => false
      case RomanNumeral.RomanDigits(_) => true
    }
  }

  property("RomanDigit can be added to a RomanNumeral") = forAll { (n: RomanNumeral, r: RomanDigit) =>
    n + r match {
      case RomanNumeral.Nulla => false
      case RomanNumeral.RomanDigits(_) => true
    }
  }

  property("RomanNumeral can be added to a RomanNumeral") = forAll { (n: RomanNumeral, m: RomanNumeral) =>
    n + m match {
      case RomanNumeral.Nulla => true
      case RomanNumeral.RomanDigits(_) => true
    }
  }

  property("RomanNumeral can be added to a RomanDigit") = forAll { (r: RomanDigit, n: RomanNumeral) =>
    r + n match {
      case RomanNumeral.Nulla => false
      case RomanNumeral.RomanDigits(_) => true
    }
  }

  property("RomanDigit can be multiplied by another") = forAll { (r: RomanDigit, s: RomanDigit) =>
    r * s match {
      case RomanNumeral.Nulla => true
      case RomanNumeral.RomanDigits(_) => true
    }
  }

  property("RomanNumeral can be multiplied by a RomanDigit") = forAll { (n: RomanNumeral, r: RomanDigit) =>
    n * r match {
      case RomanNumeral.Nulla => true
      case RomanNumeral.RomanDigits(_) => true
    }
  }

  property("RomanNumeral can be multiplied by another") = forAll { (n: RomanNumeral, m: RomanNumeral) =>
    n * m match {
      case RomanNumeral.Nulla => true
      case RomanNumeral.RomanDigits(_) => true
    }
  }

  property("RomanDigit can be multiplied by a RomanNumeral") = forAll { (r: RomanDigit, n: RomanNumeral) =>
    r * n match {
      case RomanNumeral.Nulla => true
      case RomanNumeral.RomanDigits(_) => true
    }
  }

  property("RomanDigit can be subtracted from another") = forAll { (r: RomanDigit, s: RomanDigit) =>
    r - s match {
      case RomanNumeral.Nulla => true
      case RomanNumeral.RomanDigits(_) => true
    }
  }

  property("RomanDigit can be subtracted from a RomanNumeral") = forAll { (n: RomanNumeral, r: RomanDigit) =>
    n - r match {
      case RomanNumeral.Nulla => true
      case RomanNumeral.RomanDigits(_) => true
    }
  }

  property("RomanNumeral can be subtracted from a RomanNumeral") = forAll { (n: RomanNumeral, m: RomanNumeral) =>
    n - m match {
      case RomanNumeral.Nulla => true
      case RomanNumeral.RomanDigits(_) => true
    }
  }

  property("RomanNumeral can be subtracted from a RomanDigit") = forAll { (r: RomanDigit, n: RomanNumeral) =>
    r - n match {
      case RomanNumeral.Nulla => true
      case RomanNumeral.RomanDigits(_) => true
    }
  }

  property("RomanDigit can be divided by another") = forAll { (r: RomanDigit, s: RomanDigit) =>
    r / s match {
      case RomanNumeral.Nulla => true
      case RomanNumeral.RomanDigits(_) => true
    }
  }

  property("RomanNumeral can be divided by a RomanDigit") = forAll { (n: RomanNumeral, r: RomanDigit) =>
    n / r match {
      case RomanNumeral.Nulla => true
      case RomanNumeral.RomanDigits(_) => true
    }
  }

  property("RomanNumeral can be divided by another") = forAll(genRomanNumeral, genRomanDigits) {
    (n: RomanNumeral, m: RomanNumeral) =>
      n / m match {
        case RomanNumeral.Nulla => true
        case RomanNumeral.RomanDigits(_) => true
      }
  }

  property("RomanDigit can be divided by a RomanNumeral") = forAll(genRomanDigit, genRomanDigits) {
    (r: RomanDigit, n: RomanNumeral) =>
      r / n match {
        case RomanNumeral.Nulla => true
        case RomanNumeral.RomanDigits(_) => true
      }
  }

  property("RomanNumeral division by Nulla results in ArithmeticException") = forAll { (n: RomanNumeral) =>
    Prop.throws(classOf[ArithmeticException]) { n.div(RomanNumeral.Nulla) }
  }

  property("RomanNumeral isOdd") = forAll { (n: RomanNumeral) =>
    n.isOdd == (n.toInt % 2 != 0)
  }

  property("RomanNumeral doubling then halving") = forAll { (n: RomanNumeral) =>
    n == n.double.halve
  }

  property("RomanNumeral halving then doubling") = forAll { (n: RomanNumeral) =>
    {
      if (n.isOdd) n == n.halve.double + I
      else n == n.halve.double
    }
  }

  property("RomanNumeral multiplication is commutative") = forAll { (n: RomanNumeral, m: RomanNumeral) =>
    n.times(m) == m.times(n)
  }

  property("RomanNumeral multiplication is associative") =
    forAll(genLimitedRomanNumeral, genLimitedRomanNumeral, genLimitedRomanNumeral) {
      (n: RomanNumeral, m: RomanNumeral, o: RomanNumeral) => n.times(m).times(o) == n.times(m.times(o))
    }

  property("RomanNumeral multiplication has an identity element") = forAll(genRomanNumeral) { n: RomanNumeral =>
    n * I == I * n && n * I == n
  }

  property("RomanNumeral adding is left distributive") = forAll { (n: RomanNumeral, m: RomanNumeral, o: RomanNumeral) =>
    n.times(m.plus(o)) == n.times(m).plus(n.times(o))
  }

  property("RomanNumeral adding is right distributive") = forAll {
    (n: RomanNumeral, m: RomanNumeral, o: RomanNumeral) => m.plus(o).times(n) == m.times(n).plus(o.times(n))
  }

  property("RomanNumeral adding then subtracting") = forAll { (n: RomanNumeral, m: RomanNumeral) =>
    n == n.plus(m).minus(m)
  }

  property("RomanNumeral subtracting is left distributive") = forAll {
    (n: RomanNumeral, m: RomanNumeral, o: RomanNumeral) => n.times(m.minus(o)) == n.times(m).minus(n.times(o))
  }

  property("RomanNumeral subtracting is right distributive") = forAll {
    (n: RomanNumeral, m: RomanNumeral, o: RomanNumeral) => m.minus(o).times(n) == m.times(n).minus(o.times(n))
  }

  property("RomanNumeral multiplying then dividing") = forAll { (n: RomanNumeral, m: RomanNumeral) =>
    m match {
      case RomanNumeral.Nulla => true
      case RomanNumeral.RomanDigits(_) => n == n.times(m).div(m)
    }
  }

  property("adding RomanNumerals and then converting to int is the same as converting to int and then adding") =
    forAll { (n: RomanNumeral, m: RomanNumeral) =>
      n.plus(m).toInt == n.toInt + m.toInt
    }

  property("RomanDigit comparison: <") = forAll { (r: RomanDigit, s: RomanDigit) =>
    (r < s) == (r.toInt < s.toInt)
  }

  property("RomanDigit comparison: <=") = forAll { (r: RomanDigit, s: RomanDigit) =>
    (r <= s) == (r.toInt <= s.toInt)
  }

  property("RomanDigit comparison: >") = forAll { (r: RomanDigit, s: RomanDigit) =>
    (r > s) == (r.toInt > s.toInt)
  }

  property("RomanDigit comparison: >=") = forAll { (r: RomanDigit, s: RomanDigit) =>
    (r >= s) == (r.toInt >= s.toInt)
  }

  property("RomanNumeral comparison: <") = forAll { (n: RomanNumeral, m: RomanNumeral) =>
    (n < m) == (n.toInt < m.toInt)
  }

  property("RomanNumeral comparison: <=") = forAll { (n: RomanNumeral, m: RomanNumeral) =>
    (n <= m) == (n.toInt <= m.toInt)
  }

  property("RomanNumeral comparison: >") = forAll { (n: RomanNumeral, m: RomanNumeral) =>
    (n > m) == (n.toInt > m.toInt)
  }

  property("RomanNumeral comparison: >=") = forAll { (n: RomanNumeral, m: RomanNumeral) =>
    (n >= m) == (n.toInt >= m.toInt)
  }

  property("RomanDigit to Char and back") = forAll { (r: RomanDigit) =>
    r == r.toChar.toRomanDigit.getOrElse(false)
  }

  property("RomanDigit from Char") = forAll { (c: Char) =>
    c.toRomanDigit match {
      case Some(_) => List('I', 'V', 'X', 'L', 'C', 'D', 'M').contains(c)
      case None => !List('I', 'V', 'X', 'L', 'C', 'D', 'M').contains(c)
    }
  }
  property("RomanNumeral to String and back") = forAll { (n: RomanNumeral) =>
    n == n.toString.toRomanNumeral.getOrElse(false)
  }

  property("RomanNumeral to Int and back") = forAll { (n: RomanNumeral) =>
    n == n.toInt.toRomanNumeral
  }

  property("RomanNumeral created from Roman Digits is validated") = forAll(genStringOfRomanDigits) { (s: String) =>
    s.toRomanNumeral.isEmpty || s.toRomanNumeral.nonEmpty
  }

  property("RomanNumeral created from String is validated") = forAll { (s: String) =>
    s.toRomanNumeral.isEmpty || s.toRomanNumeral.nonEmpty
  }
}
