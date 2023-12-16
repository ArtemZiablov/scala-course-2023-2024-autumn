package karazin.scala.users.group.week3

import scala.annotation.tailrec

object Homework:
  
  // Peano numbers
  abstract class Nat:
    def isZero: Boolean
    def predecessor: Nat
    def successor: Nat = Succ(this)
    
    infix def + (that: Nat): Nat
    
    infix def - (that: Nat): Nat
    
    // Optional task
    def toInt: Int

    // Optional task
    def fromInt(int: Int): Nat =
      @tailrec
      def convertFromIntToNat(num: Int, res: Nat): Nat =
        if num == 0 then res
        else convertFromIntToNat(num - 1, Succ(res))

      require(int >= 0, "Negative integer is not allowed")
      convertFromIntToNat(int, Zero)
  
    override def toString: String = s"Succ($predecessor)"
  
  type Zero = Zero.type 
  object Zero extends Nat:
    def isZero: Boolean = true
    def predecessor: Nat = throw new Exception("0 doesn't have a predecessor")

    infix def +(that: Nat): Nat = that

    infix def -(that: Nat): Nat = if !that.isZero then throw IllegalArgumentException("Wrong operation") else this
    
    // Optional task
    def toInt: Int = 0

    override def toString: String = "Zero"

    override def equals(obj: Any): Boolean = obj match
      case that: Zero =>
        (this.isZero == that.isZero) &&
          this.hashCode == that.hashCode
      case _ => false

    override def hashCode: Int = 31

  class Succ(n: Nat) extends Nat:
    def isZero: Boolean = false
    def predecessor: Nat = n
    
    infix def +(that: Nat): Nat =
      if !that.isZero then Succ(this) + that.predecessor
      else this
    
    infix def -(that: Nat): Nat =
      if !this.isZero && !that.isZero then this.predecessor - that.predecessor
      else if this.isZero then that
      else this
    
    // Optional task
    def toInt: Int =
      @tailrec
      def convertFromSuccToInt(currentNat: Nat, result: Int): Int =
        if currentNat.isZero then result
        else convertFromSuccToInt(currentNat.predecessor, result + 1)

      convertFromSuccToInt(this, 0)


    override def equals(obj: Any): Boolean = obj match
      case that: Nat =>
        (this.isZero == that.isZero) &&
          (this.predecessor == that.predecessor) &&
          this.hashCode == that.hashCode
      case _ => false

    override def hashCode: Int = 31 * predecessor.hashCode + 1
