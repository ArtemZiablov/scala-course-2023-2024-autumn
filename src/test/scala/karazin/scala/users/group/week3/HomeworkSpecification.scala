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
  import arbitraries.{given Arbitrary[Zero], given Arbitrary[Nat]}

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

  property("throw exception due to subtracting a nat from zero") = forAll { (zero: Zero, nat: Nat) =>
    throws(classOf[IllegalArgumentException]) {
      val newNat = if isZero(nat) then successor(nat) else nat
      zero - newNat
    }
  }

  property("Zero to Int") = forAll { (zero: Zero) =>
    zero.toInt == 0
  }

  property("Zero equals") = forAll { (zero1: Zero, zero2: Zero) =>
    (zero1 == zero2) && (isZero(zero1) == isZero(zero2))
  }

end ZeroSpecification


object SuccSpecification extends Properties("Succ"):
  import arbitraries.{given Arbitrary[Succ], given Arbitrary[Nat] }

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

  property("Successor`s equals") = forAll { (left: Succ, right: Succ) =>
    (left == right) == (toInt(left) == toInt(right))
  }

end SuccSpecification

object NatSpecification extends Properties("Nat"):
  import arbitraries.{given Arbitrary[Succ], given Arbitrary[Nat] }

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

  property("Nat equals") = forAll { (left: Nat, right: Nat) =>
    (left == right) == (toInt(left) == toInt(right))
  }

end NatSpecification