package com.gvolpe.demo

import shapeless._

object DeltaDemo extends App {
  case class Foo(i : Int, s : String)
  case class Bar(b : Boolean, s : String, of: Option[Bar])

  import DeltaSyntax._

  assert(6 == 2.delta(8))
  assert(("foo", "bar") == "foo".delta("bar"))
  assert(6 :: ("foo", "bar") :: HNil == (2 :: "foo" :: HNil).delta(8 :: "bar" :: HNil))
  assert(6 :: ("foo", "bar") :: HNil == Foo(2, "foo").delta(Foo(8, "bar")))

  val bar1 = Bar(true,  "foo",  Some(Bar(true, "bar",  None)))
  val bar2 = Bar(false, "food", Some(Bar(true, "barf", None)))

  println(bar1 delta bar2)  // false :: (foo,food) :: Some(true :: (bar,barf) :: None :: HNil) :: HNil

  val foo1 = Foo(3, "foo")
  val foo2 = Foo(10, "bar")

  println(foo1 delta foo2)  // 7 :: (foo,bar) :: HNil

  val fooList1 = List(Foo(3, "foo"), Foo(76, "list"))
  val fooList2 = List(Foo(87, "seq"), Foo(10, "bar"))

  val listDelta = for {
    f1 <- fooList1
    f2 <- fooList2
  } yield f1 delta f2

  println(listDelta)        // List(84 :: (foo,seq) :: HNil, 7 :: (foo,bar) :: HNil, 11 :: (list,seq) :: HNil, -66 :: (list,bar) :: HNil)

}

trait Delta[In] {
  type Out
  def apply(before: In, after: In): Out
}

trait Delta0 {
  implicit def generic[F, G]
    (implicit gen: Generic.Aux[F, G], genDelta: Lazy[Delta[G]]):
      Delta.Aux[F, genDelta.value.Out] = new Delta[F] {
    type Out = genDelta.value.Out
    def apply(before: F, after: F): Out = genDelta.value.apply(gen.to(before), gen.to(after))
  }
}

object Delta extends Delta0 {
  def apply[In](implicit delta: Lazy[Delta[In]]): Delta.Aux[In, delta.value.Out] = delta.value

  type Aux[In, Out0] = Delta[In] { type Out = Out0 }

  implicit val booleanDelta: Delta.Aux[Boolean, Boolean] = new Delta[Boolean] {
    type Out = Boolean
    def apply(before: Boolean, after: Boolean): Out = before == after
  }

  implicit val intDelta: Delta.Aux[Int, Int] = new Delta[Int] {
    type Out = Int
    def apply(before: Int, after: Int): Out = after - before
  }

  implicit val longDelta: Delta.Aux[Long, Long] = new Delta[Long] {
    type Out = Long
    def apply(before: Long, after: Long): Out = after - before
  }

  implicit def stringDelta: Delta.Aux[String, (String, String)] = new Delta[String] {
    type Out = (String, String)
    def apply(before: String, after: String): (String, String) = (before, after)
  }

  implicit def optionDelta[T](implicit deltaT: Lazy[Delta[T]]):
    Delta.Aux[Option[T], Option[deltaT.value.Out] :+: T :+: T :+: CNil] = new Delta[Option[T]] {
    type Out = Option[deltaT.value.Out] :+: T :+: T :+: CNil

    def apply(before: Option[T], after: Option[T]): Out = (before, after) match {
      case (None, None)       => Inl(None)
      case (Some(b), Some(a)) => Inl(Some(deltaT.value.apply(b, a)))
      case (Some(b), None)    => Inr(Inl(b))
      case (None, Some(a))    => Inr(Inr(Inl(a)))
    }
  }

//    implicit def listDelta[T](implicit deltaT: Lazy[Delta[T]]):
//      Delta.Aux[List[T], List[deltaT.value.Out] :+: T :+: T :+: CNil] = new Delta[List[T]] {
//      type Out = List[deltaT.value.Out] :+: T :+: T :+: CNil
//
////      import syntax.std.traversable._
//
//      def apply(before: List[T], after: List[T]): Out = {
//        val list = for {
//          b <- before
//          a <- after
//        } yield deltaT.value.apply(b, a)
//        Inl(list(0))
//      }
//    }

  implicit def deriveHNil: Delta.Aux[HNil, HNil] = new Delta[HNil] {
    type Out = HNil
    def apply(before: HNil, after: HNil): HNil = HNil
  }

  implicit def deriveHCons[H, T <: HList]
    (implicit deltaH: Delta[H], deltaT: Lazy[Delta[T] { type Out <: HList }]):
      Delta.Aux[H :: T, deltaH.Out :: deltaT.value.Out] = new Delta[H :: T] {
    type Out = deltaH.Out :: deltaT.value.Out

    def apply(before: H :: T, after: H :: T): Out = {
      deltaH(before.head, after.head) :: deltaT.value(before.tail, after.tail)
    }
  }
}

object DeltaSyntax {
//  implicit class ListDeltaOps[T](val before: List[T]) extends AnyVal {
//    def delta(after: In)(implicit delta: Lazy[Delta[In]]): delta.value.Out = delta.value(before, after)
//  }
  implicit class DeltaOps[In](val before: In) extends AnyVal {
    def delta(after: In)(implicit delta: Lazy[Delta[In]]): delta.value.Out = delta.value(before, after)
  }
}
