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
        NonEmpty(elem, left remove x, right)
      else if x > elem then
        NonEmpty(elem, left, right remove x)
      else left ∪ right

    @targetName("union")
    infix def ∪(that: IntSet): IntSet = that match
      case NonEmpty(element, left, right) => (this include element) ∪ left ∪ right
      case Empty => this


    @targetName("intersection")
    infix def ∩(that: IntSet): IntSet = that match
      case Empty => Empty
      case NonEmpty(element, left, right) =>
        if (this contains element) then NonEmpty(element, left ∩ this, right ∩ this)
        else left ∩ this ∪ right ∩ this


    @targetName("complement")
    infix def ∖(that: IntSet): IntSet = that match
      case Empty => this
      case NonEmpty(element, left, right) =>
        if that contains this.elem then (this.left ∖ that) ∪ (this.right ∖ that)
        else NonEmpty(this.elem, this.left ∖ that, this.right ∖ that)


    @targetName("disjunctive union")
    infix def ∆(that: IntSet): IntSet = that match
      case Empty => this
      case _ => (this ∖ that) ∪ (that ∖ this)


    override def toString: String = s"[$left - [$elem] - $right]"

    override def hashCode(): Int =
      val prime = 31
      prime * (prime * (prime + elem.hashCode()) + left.##) + right.##

    override def equals(other: Any): Boolean =
      other match
        case that: NonEmpty =>
          this.elem == that.elem &&
            this.left == that.left &&
            this.right == that.right &&
            this.hashCode() == that.hashCode()
        case _ => false


  end NonEmpty

end Homework
