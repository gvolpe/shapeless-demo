package com.gvolpe.demo

import shapeless._
import syntax.std.function._
import ops.function._

object Demo extends App {

  def applyProduct[P <: Product, F, L <: HList, R](p: P)(f: F)
    (implicit
     gen: Generic.Aux[P, L],
     fp: FnToProduct.Aux[F, L => R]
    ) = f.toProduct(gen.to(p))

  case class Car(brand: String, model: String, year: Int)

  val fn = (b: String, m: String, y: Int) => s"Car: $b $m of the year $y"
  val car = Car("Ford", "Focus", 2015)
  val description = applyProduct(car)(fn)
  println(description)

}
