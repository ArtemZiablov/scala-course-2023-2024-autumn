package karazin.scala.users.group.week2.homework
import scala.language.implicitConversions

import scala.math._
import org.scalacheck._
import Prop.{forAll, propBoolean, throws}
import karazin.scala.users.group.week2.homework.arbitraries
import Homework._
import karazin.scala.users.group.week2.arbitraries.restricted.{Integer, NegativeInteger, PositiveInteger, Zero}
import utils._


object HomeworkSpecification extends Properties("Homework"):

  import arbitraries.{
    given Arbitrary[Int], given Arbitrary[Rational], given Arbitrary[NegativeInteger],
    given Arbitrary[Integer], given Arbitrary[Zero], given Arbitrary[PositiveInteger]
  }


  property("throw exception due to zero denominator") = forAll { (zero: Zero) =>
    throws(classOf[IllegalArgumentException]) {
      Rational(1, zero)
    }
  }

  property("throw exception due to negative denominator") = forAll { (numer: Int, kindaDenom: NegativeInteger) =>
    throws(classOf[IllegalArgumentException]) {
      Rational(numer, kindaDenom)
    }
  }

  property("check that rational number is simplified") = forAll { (numer: Int, int: Int) =>
    val denom = abs(int) + 1
    val rational = Rational(numer, denom)

    rational.numer == (numer / gcd(abs(numer), denom)) && rational.denom == (denom / gcd(abs(numer), denom))
  }

  property("check equals") = forAll { (left: Rational, right: Rational) =>
    (left == right) == (left.numer == right.numer && left.denom == right.denom)
  }

  property("less then") = forAll { (left: Rational, right: Rational) =>
    (left < right) == (left.numer * right.denom < right.numer * left.denom)
  }

  property("less or equal") = forAll { (left: Rational, right: Rational) =>
    (left <= right) == ( left < right || left == right)
  }

  property("greater") = forAll { (left: Rational, right: Rational) =>
    (left > right) == !(left <= right)
  }

  property("greater or equal") = forAll { (left: Rational, right: Rational) =>
    (left >= right) == ( left > right || left == right)
  }

  property("negation") = forAll { (rational: Rational) =>
    val result = -rational
    (result.numer == - rational.numer) && (result.denom == rational.denom)
  }

  property("addition") = forAll { (left: Rational, right: Rational) =>
    // gcd and lcm from: https://rosettacode.org/wiki/Least_common_multiple#Scala
    def lcm(a: Int, b: Int)=(a*b).abs/gcd(a,b)

    val expectedDenom = lcm(left.denom, right.denom)
    val expectedNumer = (left.numer * (expectedDenom / left.denom)) + (right.numer * (expectedDenom / right.denom))
    val g = gcd(Math.abs(expectedNumer), expectedDenom)
    val res = (left + right)
    (res.denom == (expectedDenom / g) && res.numer == (expectedNumer / g))
  }

  property("addition with positive integer") = forAll { (left: Rational, number: PositiveInteger) =>
    (left + number) == left + Rational(number)
  }

  property("addition with negative integer") = forAll { (left: Rational, number: NegativeInteger) =>
    (left + number) == left + Rational(number)
  }

  property("addition with zero") = forAll { (left: Rational, number: Zero) =>
    (left + number) == left + Rational(number)
  }

  property("Positive int add rational") = forAll { (number: PositiveInteger, right: Rational) =>
    (number + right) == Rational(number) + right
  }

  property("Negative int add rational") = forAll { (number: NegativeInteger, right: Rational) =>
    (number + right) == Rational(number) + right
  }

  property("Zero add rational") = forAll { (number: Zero, right: Rational) =>
    (number + right) == Rational(number) + right
  }

  property("subtraction") = forAll { (left: Rational, right: Rational) =>
    // gcd and lcm from: https://rosettacode.org/wiki/Least_common_multiple#Scala
    def lcm(a: Int, b: Int) = (a * b).abs / gcd(a, b)

    val expectedDenom = lcm(left.denom, right.denom)
    val expectedNumer = (left.numer * (expectedDenom / left.denom)) - (right.numer * (expectedDenom / right.denom))
    val g = gcd(Math.abs(expectedNumer), expectedDenom)
    val res = (left - right)
    (res.denom == (expectedDenom / g) && res.numer == (expectedNumer / g))
  }

  property("subtraction with positive integer") = forAll { (left: Rational, number: PositiveInteger) =>
    (left - number) == left - Rational(number)
  }

  property("subtraction with negative integer") = forAll { (left: Rational, number: NegativeInteger) =>
    (left - number) == left - Rational(number)
  }


  property("subtraction with zero") = forAll { (left: Rational, number: Zero) =>
    (left - number) == left - Rational(number)
  }

  property("Positive int subtract rational") = forAll { (number: PositiveInteger, right: Rational) =>
    (number - right) == Rational(number) - right
  }

  property("Negative int subtract rational") = forAll { (number: NegativeInteger, right: Rational) =>
    (number - right) == Rational(number) - right
  }

  property("Zero subtract rational") = forAll { (number: NegativeInteger, right: Rational) =>
    (number - right) == Rational(number) - right
  }

  property("multiplication of rationals") = forAll { (left: Rational, right: Rational) =>
    val expectedNumer = right.numer * left.numer
    val expectedDenom = left.denom * right.denom
    val g = gcd(Math.abs(expectedNumer), expectedDenom)
    val res = (left * right)
    (res.denom == (expectedDenom / g) && res.numer == (expectedNumer / g))
  }

  property("multiplication on positive integer") = forAll { (left: Rational, number: PositiveInteger) =>
    (left * number) == left * Rational(number)
  }

  property("multiplication on negative integer") = forAll { (left: Rational, number: NegativeInteger) =>
    (left * number) == left * Rational(number)
  }

  property("multiplication on zero") = forAll { (left: Rational, number: Zero) =>
    (left * number) == left * Rational(number)
  }

  property("multiplication with positive int on rational") = forAll { (number: PositiveInteger, right: Rational) =>
    (number * right) == Rational(number) * right
  }

  property("multiplication with negative int on rational") = forAll { (number: NegativeInteger, right: Rational) =>
    (number * right) == Rational(number) * right
  }

  property("multiplication with zero on rational") = forAll { (number: Zero, right: Rational) =>
    (number * right) == Rational(number) * right
  }

  property("division rational with rational") = forAll { (left: Rational, numer: Integer, denom: Integer) =>

    val right = Rational(if numer equals 0 then 1 else numer, abs(denom) + 1)
    val positive = if right.numer > 0 then true else false
    val pair =
      if positive then (left.numer * right.denom, left.denom * right.numer)
      else (left.numer * (-right.denom), left.denom * (-right.numer))
    val g = gcd(Math.abs(pair._1), pair._2)
    val res = (left / right)
    (res.denom == (pair._2 / g) && res.numer == (pair._1 / g))
  }

  property("division with rational by positive int") = forAll { (left: Rational, number: PositiveInteger) =>
    val newLeft = if left.numer == 0 then Rational(1, left.denom) else left
    (newLeft / number) == newLeft / Rational(number)
  }

  property("division with rational by negative int") = forAll { (left: Rational, number: NegativeInteger) =>
    (left / number) == left / Rational(number)
  }

  property("division with rational by zero") = forAll { (left: Rational, right: Zero) =>
    throws(classOf[IllegalArgumentException]) {
      left / right
    }
  }

  property("division with positive int by rational") = forAll { (number: PositiveInteger, right: Rational) =>
    val newRight = if right.numer == 0 then Rational(1, right.denom) else right
    (number / newRight) == (Rational(number) / newRight)
  }

  property("division with negative int by rational") = forAll { (number: NegativeInteger, right: Rational) =>
    val newRight = if right.numer == 0 then Rational(1, right.denom) else right
    (number / newRight) == (Rational(number) / newRight)
  }

  property("division with zero by rational") = forAll { (number: Zero, right: Rational) =>
    val newRight = if right.numer == 0 then Rational(1, right.denom) else right
    (number / newRight) == (Rational(number) / newRight)
  }

end HomeworkSpecification