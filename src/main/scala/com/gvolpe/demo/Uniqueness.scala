package com.gvolpe.demo

object Uniqueness extends App {

  import shapeless._
  import ops.hlist.Filter

  implicit class Uniqueable[L <: HList](l: L) {
    def unique[A](implicit ev: Filter.Aux[L, A, A :: HNil]) = ev(l).head
  }

  class Foo
  class Bar extends Foo

  val foo = new Foo
  val bar = new Bar

  val sample = 1 :: bar :: foo :: "a" :: HNil

  // unique does not filter subtypes so this will compile
  val uniqueFoo = sample.unique[Foo]
  val uniqueBar = sample.unique[Bar]

  assert(foo == uniqueFoo)
  assert(bar == uniqueBar)

  // This won't compile because there are two elements of type Foo (Foo and Bar) in the sample
//  stuff.unifySubtypes[Foo].unique[Foo]

  val sample2 = 5 :: bar :: true :: HNil

  val uniqueBar2 = sample2.unifySubtypes[Foo].unique[Foo]

  assert(bar == uniqueBar2)

}
