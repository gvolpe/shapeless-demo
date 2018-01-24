package com.gvolpe.demo.book

import shapeless._
import shapeless.ops.hlist._

object chapterfour {

  // Dependenty Types
  val last1 = Last[String :: Int :: HNil]
  val last2 = Last[Int :: String :: HNil]

  val l1 = last1("foo" :: 123 :: HNil)
  val l2 = last2(321 :: "bar" :: HNil)

  trait Second[L <: HList] {
    type Out
    def apply(value: L): Out
  }

  object Second {
    type Aux[L <: HList, O] = Second[L] { type Out = O }
    def apply[L <: HList](implicit ev: Second[L]): Aux[L, ev.Out] = ev
  }

  import Second._

  implicit def hlistSecond[A, B, Rest <: HList]: Aux[A :: B :: Rest, B] =
    new Second[A :: B :: Rest] {
      type Out = B
      def apply(value: A :: B :: Rest): B = value.tail.head
    }

  val second1 = Second[String :: Boolean :: Int :: HNil]
  val second2 = Second[String :: Int :: Boolean :: HNil]

  val s1 = second1("foo" :: true :: 123 :: HNil)
  val s2 = second2("bar" :: 321 :: false :: HNil)

  // Some functions
  def lastField[A, Repr <: HList](input: A)(
    implicit
    gen: Generic.Aux[A, Repr],
    last: Last[Repr]
  ): last.Out = last.apply(gen.to(input))

  def getWrappedValue[A, Repr <: HList, Head](in: A)(
    implicit
    gen: Generic.Aux[A, Repr],
    isHCons: IsHCons.Aux[Repr, Head, HNil]
  ): Head = gen.to(in).head

  case class Wrapper(value: Int)

  val wrapped = getWrappedValue(Wrapper(23))

}
