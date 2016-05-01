package com.gvolpe.demo

import shapeless.PolyDefns.~>
import shapeless._
import ops.function._
import syntax.std.function._

object PolyFunctions extends App {
  import model._

  val fn = (b: String, m: String, y: Int) => s"Car: $b $m of the year $y"
  val car = Car("Ford", "Focus", 2015)
  val description = applyProduct(car)(fn)
  println(description)

  val hlist = 1 :: "Gabi" :: "gvolpe@github.com" :: HNil
//  hlist map showElement

  println(choose(Set(1, 6, 8, 3))) // Some(1)
  println(choose(Set.empty[Int])) // None

//  val setToOption: List[Option[Int]] = List(Set(4, 6), Set.empty[Int]) map choose
//  println(setToOption) // List(Some(4), None)

}

object model {
  case class Car(brand: String, model: String, year: Int)

  def applyProduct[P <: Product, F, L <: HList, R](p: P)(f: F)
    (implicit
     gen: Generic.Aux[P, L],
     fp: FnToProduct.Aux[F, L => R]
    ) = f.toProduct(gen.to(p))

//  class GenericClass[H <: HList](hs: H)
//
//  object GenericClass {
//    implicit val generic = Generic[GenericClass]
//    def apply[P <: Product, L <: HList](p: P)(implicit gen: Generic.Aux[P, L]) =
//      new GenericClass[L](gen.to(p))
//  }
}

//trait show extends Poly1 {
//  implicit def default[A] = at[A]{ a => s">> $a"}
//}

object showElement extends Poly1 {
  implicit val caseInt = at[Int]{ a => s">> $a"}
  implicit val caseString = at[String]{ a => s">> $a"}
}

object choose extends (Set ~> Option) {
  def apply[T](f: Set[T]): Option[T] = f.headOption
}


