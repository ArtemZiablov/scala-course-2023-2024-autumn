package karazin.scala.users.group.week4.homework

import scala.annotation.{tailrec, targetName}
import karazin.scala.users.group.week4.utils.ItemOrdering

object Homework:

  abstract class IntSet:

    infix def include(x: Int): IntSet

    infix def remove(x: Int): IntSet

    infix def contains(x: Int): Boolean

    @targetName("union")
    infix def ∪(that: IntSet): IntSet

    @targetName("intersection")
    infix def ∩(that: IntSet): IntSet

    @targetName("complement")
    infix def ∖(that: IntSet): IntSet

    @targetName("disjunctive union")
    infix def ∆(that: IntSet): IntSet

  end IntSet

  type Empty = Empty.type

  case object Empty extends IntSet:

    infix def include(x: Int): IntSet = NonEmpty(x, Empty, Empty)

    infix def contains(x: Int): Boolean = false

    infix def remove(x: Int): IntSet = Empty

    @targetName("union")
    infix def ∪(that: IntSet): IntSet = that

    @targetName("intersection")
    infix def ∩(that: IntSet): IntSet = this

    @targetName("complement")
    infix def ∖(that: IntSet): IntSet = this

    @targetName("disjunctive union")
    infix def ∆(that: IntSet): IntSet = that

    override def toString: String = "[*]"

    override def equals(other: Any): Boolean = other match {
      case _: Empty => true
      case _ => false
    }

    override def hashCode(): Int = 0

  end Empty

  case class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet:

    infix def include(x: Int): IntSet =
      if x < elem       then NonEmpty(elem, left include x, right)
      else if x > elem  then NonEmpty(elem, left, right include x)
      else              this

    infix def contains(x: Int): Boolean =
      if x < elem       then left contains x
      else if x > elem  then right contains x
      else              true

    // Optional task
    infix def remove(x: Int): IntSet =
      if !this.contains(x) then throw new NoSuchElementException("There is no such element in the set")
      else if x < elem then
        NonEmpty(elem, left.remove(x), right)
      else if x > elem then
        NonEmpty(elem, left, right.remove(x))
      else if left == Empty && right == Empty then Empty
      else if left == Empty then right
      else if right == Empty then left
      else
        val minElemInRight = findMinElemInRight(right)
        val newRight = right.remove(minElemInRight)
        NonEmpty(minElemInRight, left, newRight)

    @tailrec
    private def findMinElemInRight(set: IntSet): Int =
      set match {
        case nonEmptySet: NonEmpty =>
          if nonEmptySet.left == Empty then nonEmptySet.elem
          else
            findMinElemInRight(nonEmptySet.left)
        case _ => throw new NoSuchElementException("Empty set has no minimum element")
      }
    
    @targetName("union")
    infix def ∪(that: IntSet): IntSet =
      if !that.isInstanceOf[NonEmpty] then this
      else
        val newThat = that.asInstanceOf[NonEmpty]
        this.include(newThat.elem) ∪ newThat.left ∪ newThat.right
    
    @targetName("intersection")
    infix def ∩(that: IntSet): IntSet = 
      if (this.isInstanceOf[Empty]) then Empty
      else
        val nonEmptyThis = this.asInstanceOf[NonEmpty]
        if (that contains nonEmptyThis.elem)
          then NonEmpty(nonEmptyThis.elem, nonEmptyThis.left ∩ that, nonEmptyThis.right ∩ that)
        else
          nonEmptyThis.left ∩ that ∪ nonEmptyThis.right ∩ that

    @targetName("complement")
    infix def ∖(that: IntSet): IntSet =
      if !that.isInstanceOf[NonEmpty] then this
      else
        val newThat = that.asInstanceOf[NonEmpty]
        if newThat contains this.elem then (this.left ∖  newThat) ∪ (this.right ∖ newThat)
        else NonEmpty(this.elem, this.left ∖ newThat, this.right ∖ newThat)

    @targetName("disjunctive union")
    infix def ∆(that: IntSet): IntSet =
      if !that.isInstanceOf[NonEmpty] then this
      else
        (this ∖ that) ∪ (that ∖ this)

    override def toString: String = s"[$left - [$elem] - $right]"

    override def hashCode(): Int =
      val prime = 31
      prime * (prime * (prime + elem.hashCode()) + left.##) + right.##

    override def equals(other: Any): Boolean =
      other match {
        case that: NonEmpty =>
          this.elem == that.elem &&
            this.left == that.left &&
            this.right == that.right &&
            this.hashCode() == that.hashCode()
        case _ => false
      }

  end NonEmpty
  
end Homework



