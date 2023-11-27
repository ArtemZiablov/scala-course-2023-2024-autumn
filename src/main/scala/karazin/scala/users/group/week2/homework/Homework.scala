package karazin.scala.users.group.week2.homework

import scala.annotation.targetName
import scala.math.{abs, signum}

object Homework:

  // `x` and `y` are inaccessible from outside
  class Rational(x: Int, y: Int):
    // Checking the precondition. Is fails then throws `IllegalArgumentException`
    require(y > 0, "Denominator must be positive")

    def this(x: Int) = this(x, 1)

    val numer = x / g
    val denom = y / g

    // Defines an external name for a definition
    @targetName("less than")
    // Annotation on a method definition allows using the method as an infix operation
    infix def <(that: Rational): Boolean =
      this.numer * that.denom < that.numer * this.denom

    @targetName("less or equal")
    infix def <=(that: Rational): Boolean =
      this < that || this == that

    @targetName("greater than")
    infix def >(that: Rational): Boolean =
      !(this <= that)

    @targetName("greater or equal")
    infix def >=(that: Rational): Boolean =
      !(this < that)

    @targetName("addition")
    infix def +(that: Rational): Rational =
      val newDenom = Math.abs(this.denom * that.denom) / gcd(this.denom, that.denom)
      val newNumer = this.numer * (newDenom / this.denom) + that.numer * (newDenom / that.denom)
      Rational(newNumer, newDenom)

    @targetName("addition")
    infix def +(that: Int): Rational =
      val newNumer = this.numer + (this.denom * that)
      Rational(newNumer, this.denom)

    @targetName("negation")
    infix def unary_- : Rational =
      Rational(-1 * (this.numer), this.denom)

    @targetName("subtraction")
    infix def -(that: Rational): Rational =
      val newDenom: Int = Math.abs(this.denom * that.denom) / gcd(this.denom, that.denom)
      val newNumer: Int = this.numer * (newDenom / this.denom) - that.numer * (newDenom / that.denom)
      Rational(newNumer, newDenom)

    @targetName("subtraction")
    infix def -(that: Int): Rational =
      val newNumer = this.numer - (this.denom * that)
      Rational(newNumer, this.denom)

    @targetName("multiplication")
    infix def *(that: Rational): Rational =
      Rational(this.numer * that.numer, this.denom * that.denom)

    @targetName("multiplication")
    infix def *(that: Int): Rational =
      Rational(this.numer * that, this.denom)

    @targetName("division")
    infix def /(that: Rational): Rational =
      require(that.numer != 0, "division by zero")
      if that.numer < 0 then this * Rational(-that.denom, -that.numer)
      else this * Rational(that.denom, that.numer)

    @targetName("division")
    infix def /(that: Int): Rational =
      require(that != 0, "division by zero")
      Rational(this.numer, this.denom * that)

    override def toString: String = s"${this.numer}/${this.denom}"

    private def gcd(a: Int, b: Int): Int =
      if b == 0 then a else gcd(b, a % b)

    private lazy val g = gcd(abs(x), y)

    override def equals(other: Any): Boolean =
      if (!other.isInstanceOf[Rational]) then false
      else
        val that = other.asInstanceOf[Rational]
        (this.numer == that.numer) && (this.denom == that.denom)

  end Rational

end Homework


