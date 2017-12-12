package nl.gn0s1s.julius

import scala.collection.immutable.Stream

import org.scalacheck._
import org.scalacheck.Gen._
import org.scalacheck.Shrink.shrink

import RomanDigit._

object Generators {

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
}
