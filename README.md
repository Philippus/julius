# Julius - Roman Numerals

[![build](https://github.com/Philippus/julius/workflows/build/badge.svg)](https://github.com/Philippus/julius/actions/workflows/scala.yml?query=workflow%3Abuild+branch%3Amain)
[![codecov](https://codecov.io/gh/Philippus/julius/branch/main/graph/badge.svg)](https://codecov.io/gh/Philippus/julius)
![Current Version](https://img.shields.io/badge/version-1.2.0-brightgreen.svg?style=flat "1.2.0")
[![Scala Steward badge](https://img.shields.io/badge/Scala_Steward-helping-blue.svg?style=flat&logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA4AAAAQCAMAAAARSr4IAAAAVFBMVEUAAACHjojlOy5NWlrKzcYRKjGFjIbp293YycuLa3pYY2LSqql4f3pCUFTgSjNodYRmcXUsPD/NTTbjRS+2jomhgnzNc223cGvZS0HaSD0XLjbaSjElhIr+AAAAAXRSTlMAQObYZgAAAHlJREFUCNdNyosOwyAIhWHAQS1Vt7a77/3fcxxdmv0xwmckutAR1nkm4ggbyEcg/wWmlGLDAA3oL50xi6fk5ffZ3E2E3QfZDCcCN2YtbEWZt+Drc6u6rlqv7Uk0LdKqqr5rk2UCRXOk0vmQKGfc94nOJyQjouF9H/wCc9gECEYfONoAAAAASUVORK5CYII=)](https://scala-steward.org)
[![license](https://img.shields.io/badge/license-MPL%202.0-blue.svg?style=flat "MPL 2.0")](LICENSE)

Julius is a library for working with Roman Numerals in Scala.
It was started to get more comfortable with functional programming, Scala and property based testing.

It aims to:
- provide a pleasant API to the users of the library.
- provide the basic operations of addition, subtraction, multiplication and division.
- do as the Romans do: the implemented algorithms are only allowed to manipulate the Roman symbols directly, so no
converting back and forth from integers.
- use idiomatic Scala
- use functional style of programming
- use property based testing with ScalaCheck

## Installation

Julius is published for Scala 2.13 and Scala 3. To start using it add the following to your `build.sbt`:

```
libraryDependencies += "nl.gn0s1s" %% "julius" % "1.2.0"
```

## Specification
A Roman Numeral is either the value `nulla` (Latin for "none") or consists of one or more Roman Digits:

```
romanNumeral ::= nulla | romanDigit {romanDigit}
romanDigit ::= 'M' | 'D' | 'C' | 'L' | 'X' | 'V' | 'I'
```

## Constructing a Roman Numeral
There are several ways to construct a Roman Numeral.

Directly, by supplying a `List[RomanDigit]` to `RomanNumeral()`. This list is treated as an unordered collection of
Roman Digits.

After importing `RomanNumeral.RomanNumeralFromInt` or `RomanNumeral.RomanNumeralFromString` the following ways are
available:

Using the method `toRomanNumeral` on `Int` (resulting in a `RomanNumeral`) or `String` which will result in an
`Option[RomanNumeral]`.
Note that this last method expects compacted Roman Numerals (f.e. IV instead of IIII).

```scala
import nl.gn0s1s.julius.RomanDigit._
import nl.gn0s1s.julius.RomanNumeral
RomanNumeral(List(I, I, X, I)) // res0: RomanNumeral = XIII
import nl.gn0s1s.julius.RomanNumeral.RomanNumeralFromInt
1666.toRomanNumeral // res1: RomanNumeral = MDCLXVI
import nl.gn0s1s.julius.RomanNumeral.RomanNumeralFromString
"XIV".toRomanNumeral // res2: Option[RomanNumeral] = Some(XIV)
```

## Nulla
The special value `nulla` (zero) can also be constructed similarly:

```scala
import nl.gn0s1s.julius.RomanNumeral
RomanNumeral() // res3: RomanNumeral = nulla
import nl.gn0s1s.julius.RomanNumeral.RomanNumeralFromInt
0.toRomanNumeral // res4: RomanNumeral = nulla
import nl.gn0s1s.julius.RomanNumeral.RomanNumeralFromString
"nulla".toRomanNumeral // res5: Option[RomanNumeral] = Some(nulla)
```

## Operators and expressions
Julius makes the `+`, `-`, `*` and `/` operators available for Roman Digits and Numerals, which can be freely combined
to construct expressions. This is also yet another way to construct a Roman Numeral.

some examples:
```scala
import nl.gn0s1s.julius.RomanDigit._
import nl.gn0s1s.julius.RomanNumeral.RomanNumeralOps
M + M + X + V + I // res6: RomanNumeral = MMXVI
import nl.gn0s1s.julius.RomanNumeral.RomanNumeralFromInt
(3.toRomanNumeral * C / V) - L - X // res7: RomanNumeral = nulla
import nl.gn0s1s.julius.RomanNumeral.RomanNumeralFromString
"XX".toRomanNumeral.get * V // res8: RomanNumeral = C
```

## Links
Roman Numerals:
- General information - https://en.wikipedia.org/wiki/Roman_numerals
- Algorithms - http://turner.faculty.swau.edu/mathematics/materialslibrary/roman/
- Algorithms - http://scienceblogs.com/goodmath/2006/08/16/roman-numerals-and-arithmetic/

Scala related references:
- ScalaCheck - https://scalacheck.org/
- Sealed traits - http://underscore.io/blog/posts/2015/06/02/everything-about-sealed.html

## License
The code is available under the [Mozilla Public License, version 2.0](LICENSE).
