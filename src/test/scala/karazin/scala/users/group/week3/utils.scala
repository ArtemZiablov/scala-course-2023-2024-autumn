package karazin.scala.users.group.week3
import Homework.{Nat, Succ, Zero}
import scala.annotation.tailrec

object utils:

  def isZero(nat: Nat): Boolean =
    if toInt(nat) == 0 then true else false

  def toInt(nat: Nat): Int =
    @tailrec
    def convertFromSuccToInt(currentNat: Nat, result: Int): Int =
      if currentNat.isZero then result
      else convertFromSuccToInt(currentNat.predecessor, result + 1)

    if nat.isInstanceOf[Zero] then 0
    else
      convertFromSuccToInt(nat, 0)

  def fromInt(int: Int): Nat =
    @tailrec
    def convertFromIntToNat(num: Int, res: Nat): Nat =
      if num == 0 then res
      else convertFromIntToNat(num - 1, Succ(res))

    require(int >= 0, "Negative integer is not allowed")
    convertFromIntToNat(int, Zero)

  def predecessor(nat: Nat): Nat =
    if nat.isInstanceOf[Zero] then throw new Exception("0 doesn't have a predecessor")
    else fromInt(toInt(nat) - 1)

  def successor(nat: Nat): Nat =
    fromInt(toInt(nat) + 1)