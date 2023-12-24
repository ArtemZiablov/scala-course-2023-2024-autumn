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
      val newDenom = this.denom * that.denom
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
      val newDenom: Int = this.denom * that.denom
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
      Rational(this.numer * that.sign, abs(this.denom * that))

    override def toString: String = s"${this.numer}/${this.denom}"

    private def gcd(a: Int, b: Int): Int =
      if b == 0 then a else gcd(b, a % b)

    private lazy val g = gcd(abs(x), y)
    
    def canEqual(other: Any): Boolean = other.isInstanceOf[Rational]
    
    override def equals(other: Any): Boolean = other match {
      case that: Rational =>
        (that canEqual this) &&
          numer == that.numer &&
          denom == that.denom &&
          this.hashCode() == that.hashCode()
      case _ => false
    }
    
    override def hashCode(): Int = {
      val state = Seq(numer, denom)
      state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
    }
    
  end Rational

  given int2Rational: Conversion[Int, Rational] with
    def apply(int: Int): Rational = new Rational(int)

  extension (int: Int)
    @targetName("addition")
    def +(that: Rational): Rational = int2Rational(int) + that

    @targetName("subtraction")
    def -(that: Rational): Rational = int2Rational(int) - that

    @targetName("multiplication")
    def *(that: Rational): Rational = int2Rational(int) * that

    @targetName("division")
    def /(that: Rational): Rational = int2Rational(int) / that

  
end Homework