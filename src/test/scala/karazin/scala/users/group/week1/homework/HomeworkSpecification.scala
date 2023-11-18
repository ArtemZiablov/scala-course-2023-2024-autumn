package karazin.scala.users.group.week1.homework
import annotation.tailrec
import org.scalacheck._
import org.scalacheck.Prop.{forAll, propBoolean, throws}
import Homework._
import karazin.scala.users.group.week1.homework.arbitraries

object HomeworkSpecification extends Properties("Homework"):

  include(BooleanOperatorsSpecification)
  include(FermatNumbersSpecification)
  include(LookAndSaySequenceSpecification)

end HomeworkSpecification


object BooleanOperatorsSpecification extends Properties("Boolean Operators"):
  import `Boolean Operators`._

  property("not") = forAll { (b: Boolean) =>
    not(b) == !b
  }

  property("and") = forAll { (pair: (Boolean, Boolean)) =>
    val (left, right) = pair

    and(left, right) == (left && right)
  }

  property("or") = forAll { (pair: (Boolean, Boolean)) =>
    val (left, right) = pair

    or(left, right) == left || right
  }

end BooleanOperatorsSpecification


object FermatNumbersSpecification extends Properties("Fermat Numbers"):
  import `Fermat Numbers`._
  import arbitraries.given Arbitrary[Int]

  property("multiplication") = forAll { (left: Int, right: Int) =>
    multiplication(left, right) == (left * right)
  }

  property("Negative exponent is not allowed") = forAll { (baseNumber: Int, exponent: Int) =>
    throws(classOf[IllegalArgumentException]) {
      val newExponent = if exponent != 0 then -Math.abs(exponent) else -1
      power(baseNumber, newExponent)
    }
  }

  property("power") = forAll { (left: Int, right: Int) =>
    power(left, right) == BigInt(left).pow(right)
  }

  property("Negative n is not allowed") = forAll { (n: Int) =>
    throws(classOf[IllegalArgumentException]) {
      val newN = if n == 0 then -1 else -n
      fermatNumber(newN)
    }
  }

  property("fermatNumber") = forAll { (n: Int) =>
    fermatNumber(n) == BigInt(2).pow(BigInt(2).pow(n).toInt) + 1
  }

end FermatNumbersSpecification


object LookAndSaySequenceSpecification extends Properties("LookAndSaySequenceSpecification") {
  import `Look-and-say Sequence`._
  import arbitraries.given Arbitrary[Int]

  // https://rosettacode.org/wiki/Look-and-say_sequence#Scala
  @tailrec
  private def loop(n: Int, num: String): String = {
    if (n <= 0) num else loop(n - 1, lookAndSay(num))
  }

  private def lookAndSay(number: String): String = {
    val result = new StringBuilder

    @tailrec
    def loop(numberString: String, repeat: Char, times: Int): String =
      if (numberString.isEmpty) result.toString()
      else if (numberString.head != repeat) {
        result.append(times).append(repeat)
        loop(numberString.tail, numberString.head, 1)
      } else loop(numberString.tail, numberString.head, times + 1)

    loop(number.tail + " ", number.head, 1)
  }

  property("Argument n should be greater then zero") = forAll { (n: Int) =>
    throws(classOf[IllegalArgumentException]) {
      val newN = if n == 0 then 0 else -n
      lookAndSaySequenceElement(newN)
    }
  }

  property("lookAndSaySequenceElement") = forAll { (n: Int) =>
    (n > 0) ==> (lookAndSaySequenceElement(n).toString() == loop(n - 1, "1"))
  }

}
