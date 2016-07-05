package com.gvolpe.demo

import com.gvolpe.demo.copy.copySyntax._

object CopyTrait extends App {

  sealed trait Thing {
    val id: String
  }

  case class Demo1(id: String, name: String) extends Thing
  case class Demo2(id: String, value: Int) extends Thing

  val demo: Thing = Demo2("demo2", 5)

  println(demo)

  val demoCopy: Thing = demo.copy(id = "copy of demo2")

  println(demoCopy)

}
