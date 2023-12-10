package karazin.scala.users.group.week2.homework
import scala.language.implicitConversions
import scala.math.*
import org.scalacheck.*
import Prop.{forAll, propBoolean, throws}
import karazin.scala.users.group.week2.homework.arbitraries
import Homework.*
import karazin.scala.users.group.week2.homework.HomeworkSpecification.property
import utils.*
import arbitraries.restricted.*

object HomeworkSpecification extends Properties("Homework"):
  import arbitraries.{given Arbitrary[Int], given Arbitrary[Rational], given Arbitrary[NegativeInteger],
    given Arbitrary[Integer], given Arbitrary[Zero]}

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

    val expectedNumer = left.numer + (left.denom * number)
    val expectedDenom = left.denom
    val res = (left + number)
    val g = gcd(Math.abs(expectedNumer), expectedDenom)
    (res.numer == (expectedNumer / g)) && (res.denom == (expectedDenom  / g))
  }

  property("addition with negative integer") = forAll { (left: Rational, number: NegativeInteger) =>

    val expectedNumer = left.numer + (left.denom * number)
    val expectedDenom = left.denom
    val res = (left + number)
    val g = gcd(Math.abs(expectedNumer), expectedDenom)
    (res.numer == (expectedNumer / g)) && (res.denom == (expectedDenom / g))
  }

  property("addition with zero") = forAll { (left: Rational, number: Zero) =>
    val res = (left + number)
    (res.numer == left.numer) && (res.denom == left.denom)
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
    val expectedNumer = left.numer - (left.denom * number)
    val expectedDenom = left.denom
    val res = (left - number)
    val g = gcd(Math.abs(expectedNumer), expectedDenom)
    (res.numer == (expectedNumer / g)) && (res.denom == (expectedDenom / g))
  }

  property("subtraction with negative integer") = forAll { (left: Rational, number: NegativeInteger) =>
    val expectedNumer = left.numer - (left.denom * number)
    val expectedDenom = left.denom
    val res = (left - number)
    val g = gcd(Math.abs(expectedNumer), expectedDenom)
    (res.numer == (expectedNumer / g)) && (res.denom == (expectedDenom / g))
  }

  property("subtraction with zero") = forAll { (left: Rational, number: Zero) =>
    val res = (left - number)
    (res.numer == left.numer) && (res.denom == left.denom)
  }

  property("multiplication of rationals") = forAll { (left: Rational, right: Rational) =>
      val expectedNumer = right.numer * left.numer
      val expectedDenom = left.denom * right.denom
      val g = gcd(Math.abs(expectedNumer), expectedDenom)
      val res = (left * right)
      (res.denom == (expectedDenom / g) && res.numer == (expectedNumer / g))
  }

  property("multiplication on positive integer") = forAll { (left: Rational, number: PositiveInteger) =>
    val right = Rational(number, 1)
    val expectedNumer = left.numer * right.numer
    val expectedDenom = left.denom
    val res = (left * number)
    val g = gcd(Math.abs(expectedNumer), expectedDenom)
    (res.numer == (expectedNumer / g)) && (res.denom == (expectedDenom / g))
  }

  property("multiplication on negative integer") = forAll { (left: Rational, number: NegativeInteger) =>
    val right = Rational(number, 1)
    val expectedNumer = left.numer * right.numer
    val expectedDenom = left.denom
    val res = (left * number)
    val g = gcd(Math.abs(expectedNumer), expectedDenom)
    (res.numer == (expectedNumer / g)) && (res.denom == (expectedDenom / g))
  }

  property("multiplication on zero") = forAll { (left: Rational, number: Zero) =>
    val res = (left * number)
    (res.numer == 0) && (res.denom == 1)
  }

  /*property("division") = forAll { (left: Rational, numer: Int, denom: Int) =>
    val right = Rational(if numer == 0 then 1 else numer, abs(denom) + 1)

    val positive = if right.numer > 0 then true else false
    val pair =
      if positive then (left.numer * right.denom, left.denom * right.numer)
      else (left.numer * (-right.denom), left.denom * (-right.numer))
    val g = gcd(Math.abs(pair._1), pair._2)
    val res = (left / right)
    (res.denom == (pair._2 / g) && res.numer == (pair._1 / g))
  }*/

  property("division with positive right rational") = forAll { (left: Rational, numer: PositiveInteger, denom: PositiveInteger) =>
    val right = Rational(numer, denom)
    val pair = (left.numer * right.denom, left.denom * right.numer)
    val g = gcd(Math.abs(pair._1), pair._2)
    val res = (left / right)
    (res.denom == (pair._2 / g) && res.numer == (pair._1 / g))
  }

  property("division with negative right rational") = forAll { (left: Rational, numer: NegativeInteger, denom: PositiveInteger) =>
    val right = Rational(numer, denom)
    val pair = (left.numer * (-right.denom), left.denom * (-right.numer))
    val g = gcd(Math.abs(pair._1), pair._2)
    val res = (left / right)
    (res.denom == (pair._2 / g) && res.numer == (pair._1 / g))
  }

  property("division by zero") = forAll { (left: Rational, right: Zero) =>
    throws(classOf[IllegalArgumentException]) {
      left / right
    }
  }

end HomeworkSpecification