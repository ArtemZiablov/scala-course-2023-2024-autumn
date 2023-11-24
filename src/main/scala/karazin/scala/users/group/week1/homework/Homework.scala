package karazin.scala.users.group.week1.homework

import java.lang
import scala.annotation.tailrec
import scala.language.postfixOps
/**
 * Preface
 * Implement all the things with ???.
 * All implementations must be tail-recursive is possible.
 * Feel free to use internal methods/functions.
 * Feel free to adjust signatures of hacked methods/functions.
 *
 * 1. Boolean Operators
 * Required task
 * Implement eager boolean operations Not, And and Or.
 * Requirements:
 * a) the function should not use embedded boolean operations
 * b) the functions should be eager
 * c) the function should use `if` expression and `true` and `false` boolean literals 
 *
 * 2. Fermat Numbers
 * Required task
 * Implement function which should calculate n-th Fermat number:
 * Fn = 2 ^ (2 ^ n) + 1, n is non-negative
 * Requirements:
 * a) the function should not use multiplication and power operations
 * b) the functions should only use addition operation
 * For more details @see https://en.wikipedia.org/wiki/Fermat_number
 *
 * 3. Look-and-say Sequence
 * Required task
 * Implement function which should calculate n-th number of Look-and-say sequence
 * For more details @see https://en.wikipedia.org/wiki/Look-and-say_sequence
 *
 * 4. Kolakoski sequence
 * Optional super challenging task
 * Implement function which should calculate n-th number of Kolakoski sequence
 * For more details @see https://en.wikipedia.org/wiki/Kolakoski_sequence
 */

object Homework :

  object `Boolean Operators` :

    def not(b: Boolean): Boolean =
      if b then false
      else true


    def and(left: => Boolean, right: => Boolean): Boolean =
      if left then right
      else false


    def or(left: => Boolean, right: => Boolean): Boolean =
      if left then true
      else right

  end `Boolean Operators`

  object `Fermat Numbers` :

    val multiplication: (BigInt, BigInt) =>  BigInt =  (firstNum, secondNum) =>
      @tailrec
      def multiplicationTailRec(firstNum: BigInt, secondNum: BigInt, result: BigInt): BigInt =
        if firstNum == 0 || secondNum == 0 then result
        else if secondNum < 0 then multiplicationTailRec(firstNum, secondNum + 1, result - firstNum)
        else multiplicationTailRec(firstNum, secondNum - 1, result + firstNum)

      multiplicationTailRec(firstNum, secondNum, 0)

    val power: (BigInt, BigInt) => BigInt = (baseNumber, exponent) =>
      @tailrec
      def powerRec(baseNumber: BigInt, exponent: BigInt, result: BigInt): BigInt =
        if exponent == 0 then 1
        else if exponent == 1 then result
        else powerRec(baseNumber, exponent - 1, multiplication(result, baseNumber))

      require(exponent >= 0, "Negative exponent is not allowed")
      powerRec(baseNumber, exponent, baseNumber)


    val fermatNumber: Int => BigInt = (n) =>
      require(n >= 0, "Negative n is not allowed")
      power(2, power(2, n)) + 1

  end `Fermat Numbers`


  
  object `Look-and-say Sequence` :

    val lookAndSaySequenceElement: Int => BigInt = (n) =>

      val digits: BigInt => Int = (number) => number.toString.length()

      val constructResult: (BigInt, Int, BigInt) => BigInt = (currentDigit, count, res) =>
        (count * BigInt(10).pow(digits(count)) + currentDigit) * BigInt(10).pow(digits(res)) + res

      @tailrec
      def nextNumber(number: BigInt, currentDigit: BigInt, count: Int, res: BigInt): BigInt =
        if number == 0 then constructResult(currentDigit, count, res) / 10
        else
          val newCurrent = number % 10
          val newCount = if currentDigit != newCurrent then 1 else count + 1
          val newRes =
            if newCurrent != currentDigit then constructResult(currentDigit, count, res)
            else res
          nextNumber(number / 10, newCurrent, newCount, newRes)

      @tailrec
      def getLookAndSayElementN(n: Int, current: BigInt, count: Int): BigInt =
        if count >= n then current
        else getLookAndSayElementN(n, nextNumber(current, current % 10, 0, 0), count + 1)

      require(n > 0, "Argument n should be greater then zero")
      getLookAndSayElementN(n, BigInt(1), 1)
    
  end `Look-and-say Sequence`



  object `Kolakoski sequence`:

    val kolakoski: Int => Int = (n) =>
      @tailrec
      def getElem(sequence: String, n: BigInt, currentElementIndex: Int): Int =
        if sequence.length() >= n then
          sequence((n - 1).toInt).asDigit
        else
          val newSubSequence =
            if sequence(currentElementIndex) == '2' then
              if sequence.last.toString == "2" then "11" else "22"
            else
              if sequence.last.toString == "2" then "1" else "2"
          val newSequence = sequence + newSubSequence
          getElem(newSequence, n, currentElementIndex + 1)

      require(n > 0, "Parameter n should be greater than zero")
      getElem("122", n, 2)

  end `Kolakoski sequence`

end Homework
