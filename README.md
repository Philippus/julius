Julius - Roman Numerals
=======================

Julius is a library for working with Roman Numerals in Scala.
It was started to get more comfortable with functional programming, Scala and property based testing.

It aims to:
- provide a pleasant API to the users of the library.
- provide the basic operations of addition, subtraction, multiplication and division.
- do as the romans do: the implemented algorithms are only allowed to manipulate the roman symbols directly, so no converting back and forth from integers.
- use idiomatic Scala
- use functional style of programming
- use property based testing with ScalaCheck

[![Build Status](https://travis-ci.org/Philippus/julius.svg?branch=master)](https://travis-ci.org/Philippus/julius)
[![codecov](https://codecov.io/gh/Philippus/julius/branch/master/graph/badge.svg)](https://codecov.io/gh/Philippus/julius)

## Constructing a Roman Numeral
There are several ways to construct a Roman Numeral.

Directly, by supplying a `List[RomanDigit]`. This list is treated as an unordered collection of Roman Digits.

```scala
RomanNumeral(List(X, I, I, I))
```

After importing `JuliusImplicits._` the following ways are available:

Using the method `toRomanNumeral` on `Int` (resulting in a `RomanNumeral`) or `String` which will result in an `Option[RomanNumeral]`.
Note that this last method expects compacted Roman Numerals (f.e. IV instead of IIII).

```scala
import JuliusImplicits._
1666.toRomanNumeral
"XIV".toRomanNumeral
```

## Nulla
The special value `Nulla` (zero) can also be constructed similarly:

```scala
RomanNumeral()
import JuliusImplicits._
"nulla".toRomanNumeral
0.toRomanNumeral
```

## Usage
Julius makes the `+`, `-`, `*` and `/` operators available for Roman Numerals.

## Links
Roman Numerals:
- General information - https://en.wikipedia.org/wiki/Roman_numerals
- Algorithms - http://turner.faculty.swau.edu/mathematics/materialslibrary/roman/
- Algorithms - http://scienceblogs.com/goodmath/2006/08/16/roman-numerals-and-arithmetic/

Scala related references:
- ScalaCheck - https://scalacheck.org/
- Sealed traits - http://underscore.io/blog/posts/2015/06/02/everything-about-sealed.html

## License
The code is available under the [MIT license](LICENSE.md).
