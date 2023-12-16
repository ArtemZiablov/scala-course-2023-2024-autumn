package karazin.scala.users.group.week3

import scala.math.*
import org.scalacheck.{Arbitrary, *}
import Prop.{forAll, propBoolean, throws}
import Homework.*
import karazin.scala.users.group.week3.arbitraries.succ
import utils._

object HomeworkSpecification extends Properties("Homework"):

  include(ZeroSpecification)
  include(SuccSpecification)
  include(NatSpecification)

end HomeworkSpecification


object ZeroSpecification extends Properties("Zero"):
  import arbitraries.{given Arbitrary[Zero], given Arbitrary[Nat], given Arbitrary[Succ]}

  property("check isZero") = forAll { (zero: Zero) =>
    zero.isZero
  }

  property("throws exception due to getting predecessor of zero") = forAll { (zero: Zero) =>
    throws(classOf[Exception]) {
      zero.predecessor
    }
  }

  property("add Nat to zero") = forAll { (zero: Zero, nat: Nat) =>
    (zero + nat) == nat
  }

  property("subtract zero from zero") = forAll { (zero: Zero) =>
    (zero - zero) == Zero
  }

  property("throw exception due to subtracting a successor from zero") = forAll { (zero: Zero, succ: Succ) =>
    throws(classOf[IllegalArgumentException]) {
      zero - succ
    }
  }

  property("Zero to Int") = forAll { (zero: Zero) =>
    zero.toInt == 0
  }

  property("Zero from Int") = forAll { (int: Int) =>
    val newZero = if int != 0 then 0 else int
    Zero.fromInt(newZero) == fromInt(0)
  }

  property("Zero equals") = forAll { (zero1: Zero, zero2: Zero) =>
    (zero1 == zero2) && (isZero(zero1) == isZero(zero2))
  }

end ZeroSpecification


object SuccSpecification extends Properties("Succ"):
  import arbitraries.{given Arbitrary[Succ], given Arbitrary[Nat], given Arbitrary[Zero] }

  property("Successor is not Zero") = forAll { (succ: Succ) =>
    !succ.isZero
  }

  property("Successor`s predecessor") = forAll { (succ: Succ) =>
    succ.predecessor == predecessor(succ)
  }

  property("Successor`s addition") = forAll { (left: Succ, right: Succ) =>
    val sum = left + right
    sum.toInt == (toInt(left) + toInt(right))
  }

  property("Subtraction of to successors, where left is greater") = forAll { (left: Succ, right: Succ) =>
    if left.toInt >= right.toInt then
      toInt(left - right) == (toInt(left) - toInt(right))
    else
      toInt(right - left) == (toInt(right) - toInt(left))
  }

  property("Subtraction of to successors, throws an exception where right is greater") = forAll { (left: Succ, right: Succ) =>
    throws(classOf[IllegalArgumentException]) {
      if left.toInt > right.toInt then
        toInt(right - left) == (toInt(right) - toInt(left))
      else
        val newRight = Succ(right)
        toInt(left - newRight) == (toInt(left) - toInt(newRight))
    }
  }

  property("Succ subtract Zero") = forAll { (succ: Succ, zero: Zero) =>
    (succ - zero) == succ
  }

  property("Succ to Int") = forAll { (succ: Succ) =>
    succ.toInt == toInt(succ)
  }

  property("Successor`s equals") = forAll { (left: Succ, right: Succ) =>
    (left == right) == (toInt(left) == toInt(right))
  }

end SuccSpecification

object NatSpecification extends Properties("Nat"):
  import arbitraries.{given Arbitrary[Succ], given Arbitrary[Nat], given Arbitrary[Int] }

  property("Check if Nat is Zero") = forAll { (nat: Nat) =>
    if toInt(nat) == 0 then isZero(nat) else !isZero(nat)
  }

  property("Nat predecessor") = forAll { (nat: Nat) =>
    if toInt(nat) != 0 then nat.predecessor == fromInt(nat.toInt - 1)
    else
      throws(classOf[Exception]) {
        nat.predecessor
      }
  }

  property("Nat successor") = forAll { (nat: Nat) =>
    nat.successor == successor(nat)
  }

  property("Nat addition") = forAll { (left: Nat, right: Nat) =>
    (left + right) == fromInt(toInt(left) + toInt(right))
  }

  property("Nat subtraction where left is greater") = forAll { (left: Nat, right: Nat) =>
    if toInt(left) >= toInt(right) then
      (left - right) == fromInt(toInt(left) - toInt(right))
    else
      (right - left) == fromInt(toInt(right) - toInt(left))
  }

  property("Nat subtraction throws exception where right is greater") = forAll { (left: Nat, right: Nat) =>
    throws(classOf[IllegalArgumentException]) {
      if toInt(left) < toInt(right) then left - right
      else right - successor(left)
    }
  }

  property("Nat to Int") = forAll { (nat: Nat) =>
    nat.toInt == toInt(nat)
  }

  property("Nat from positive Int") = forAll { (int: Int) =>
    val newInt = if int < 0 then abs(int) else int
    Zero.fromInt(newInt) == fromInt(newInt)
  }

  property("Nat from negative Int throws exception") = forAll { (int: Int) =>
    throws(classOf[IllegalArgumentException]){
      val newInt =
        if int < 0 then int
        else if int > 0 then -int
        else -1
      Zero.fromInt(newInt)
    }
  }

  property("Nat equals") = forAll { (left: Nat, right: Nat) =>
    (left == right) == (toInt(left) == toInt(right))
  }

end NatSpecification