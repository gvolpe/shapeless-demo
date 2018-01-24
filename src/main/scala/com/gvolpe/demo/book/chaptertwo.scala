package com.gvolpe.demo.book

import shapeless._

object chaptertwo {

  // Generic Product encodings
  case class IceCream(name: String, numCherries: Int, inCone: Boolean)

  val iceCreamGen = Generic[IceCream]

  val iceCream    = IceCream("Sundae", 1, false)
  val repr        = iceCreamGen.to(iceCream)
  val iceCream2   = iceCreamGen.from(repr)

  case class Employee(name: String, number: Int, manager: Boolean)
  val employee = Generic[Employee].from(Generic[IceCream].to(iceCream))

  // Generic CoProduct encodings
  case class Red()
  case class Yellow()
  case class Green()

  type Light = Red :+: Yellow :+: Green :+: CNil

  val red: Light = Inl(Red())
  val green: Light = Inr(Inr(Inl(Green())))

  sealed trait Shape
  final case class Rectangle(width: Double, height: Double) extends Shape
  final case class Circle(radius: Double) extends Shape

  val gen = Generic[Shape]

  val rectangle = gen.to(Rectangle(3.0, 4.0))
  val circle    = gen.to(Circle(1.0))

}
