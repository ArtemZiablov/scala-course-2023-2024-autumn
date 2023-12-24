package karazin.scala.users.group.week3

import org.scalacheck._
import Gen.lzy
import Homework.{Nat, Succ, Zero}
import scala.language.implicitConversions


object arbitraries:

  object restricted:
    opaque type Integerable = Int
    opaque type ZeroInteger <: Integerable = Int
    opaque type PositiveInteger <: Integerable = Int
    opaque type NegativeInteger <: Integerable = Int
    type Integer = PositiveInteger | ZeroInteger | NegativeInteger

    object ZeroInteger:
      def apply(v: Int): ZeroInteger =
        require(v == 0, s"Cannot build Zero from [$v]");
        v

    object PositiveInteger:
      def apply(v: Int): PositiveInteger =
        require(v > 0, s"Cannot build positive integer from [$v]");
        v

    object NegativeInteger:
      def apply(v: Int): NegativeInteger =
        require(v < 0, s"Cannot build negative integer from [$v]");
        v

    object Integer:
      def apply(v: PositiveInteger | ZeroInteger | NegativeInteger): Integer = v

    implicit val integerableToInt: Conversion[Integerable, Int] = _.toInt

  import restricted._

  val zeroGen: Gen[ZeroInteger] = Gen.const(ZeroInteger(0))
  val positiveIntegerGen: Gen[PositiveInteger] = Gen.choose(min = 1, max = 1000) map { PositiveInteger(_) }
  val negativeIntegerGen: Gen[NegativeInteger] = Gen.choose(min = -1000, max = -1) map { NegativeInteger(_) }
  val integerGen: Gen[Integer] = Gen.frequency((2, negativeIntegerGen), (1, zeroGen), (2, positiveIntegerGen))

  given Arbitrary[ZeroInteger] = Arbitrary(zeroGen)
  given Arbitrary[PositiveInteger] = Arbitrary(positiveIntegerGen)
  given Arbitrary[NegativeInteger] = Arbitrary(negativeIntegerGen)
  given Arbitrary[Integer] = Arbitrary(integerGen)
  
  
  val zero: Gen[Zero] = Gen.const(Zero)

  def succ(nat: Nat = Zero): Gen[Succ] = 
    Gen.frequency((1, Gen.const(Succ(nat))), (3, lzy(succ(Succ(nat)))))

  val nat: Gen[Nat] = Gen.frequency((1, zero), (4, succ()))

  given Arbitrary[Zero] = Arbitrary(zero)
  given Arbitrary[Succ] = Arbitrary(succ())
  given Arbitrary[Nat] = Arbitrary(nat)
  given Arbitrary[Int] = Arbitrary(Gen.choose[Int](min = -1000, max = 1000))