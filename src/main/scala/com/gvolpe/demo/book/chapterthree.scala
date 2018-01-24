package com.gvolpe.demo.book

import com.gvolpe.demo.book.chaptertwo.{Shape, Rectangle, Circle}
import shapeless._
import shapeless.ops.hlist._

object chapterthree {

  // Automatic typeclass derivation
  trait CsvEncoder[A] {
    def encode(value: A): List[String]
  }

  object CsvEncoder {
    def apply[A](implicit ev: CsvEncoder[A]): CsvEncoder[A] = ev
  }

  def writeCsv[A](values: List[A])(implicit enc: CsvEncoder[A]): String =
    values.map(value => enc.encode(value).mkString(",")).mkString("\n")

  //val implicitInstance = the[CsvEncoder[IceCream]]

  def createEncoder[A](f: A => List[String]): CsvEncoder[A] =
    new CsvEncoder[A] {
      def encode(value: A): List[String] = f(value)
    }

  implicit val stringEncoder: CsvEncoder[String] = createEncoder(x => List(x))
  implicit val intEncoder: CsvEncoder[Int] = createEncoder(x => List(x.toString))
  implicit val booleanEncoder: CsvEncoder[Boolean] = createEncoder(x => List(if(x) "yes" else "no"))
  implicit val doubleEncoder: CsvEncoder[Double] = createEncoder(x => List(x.toString))

  implicit val hnilEncoder: CsvEncoder[HNil] = createEncoder(hnil => Nil)
  implicit def hlistEncoder[H, T <: HList](
    implicit
    hEncoder: Lazy[CsvEncoder[H]],  // For recursive types (Binary tree case)
    tEncoder: CsvEncoder[T]
  ): CsvEncoder[H :: T] =
    createEncoder {
      case h :: t => hEncoder.value.encode(h) ++ tEncoder.encode(t)
    }

  val reprEncoder: CsvEncoder[String :: Int :: Boolean :: HNil] = implicitly
  val example = reprEncoder.encode("abc" :: 123 :: true :: HNil)

  // Generic Product encoder
  implicit def genericEncoder[A, R](
    implicit
    gen: Generic.Aux[A, R],
    env: Lazy[CsvEncoder[R]] // For recursive types
  ): CsvEncoder[A] =
    createEncoder(a => env.value.encode(gen.to(a)))

  // Generic CoProduct encoder
  implicit val cnilEncoder: CsvEncoder[CNil] = createEncoder(cnil => throw new Exception("Inconceivable!"))
  implicit def coproductEncoder[H, T <: Coproduct](
    implicit
    hEncoder: Lazy[CsvEncoder[H]], // For recursive types (Binary tree case)
    tEncoder: CsvEncoder[T]
  ): CsvEncoder[H :+: T] = createEncoder {
    case Inl(h) => hEncoder.value.encode(h)
    case Inr(t) => tEncoder.encode(t)
  }

  val shapes: List[Shape] = List(
    Rectangle(3.0, 4.0),
    Circle(1.0)
  )

  val csvShapes = writeCsv(shapes)

  // Recursive Types: Lazy
  sealed trait Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  case class Leaf[A](value: A) extends Tree[A]

  val treeInstance = CsvEncoder[Tree[Int]]

  // Exercise
  trait MyTC[A]

  implicit def intInstance: MyTC[Int] = new MyTC[Int] {}
  implicit def stringInstance: MyTC[String] = new MyTC[String] {}
  implicit def booleanInstance: MyTC[Boolean] = new MyTC[Boolean] {}

  implicit def hnilInstance: MyTC[HNil] = new MyTC[HNil] {}
  implicit def hlistInstance[H, T <: HList](
    implicit
    hInstance: Lazy[MyTC[H]], // wrap in Lazy
    tInstance: MyTC[T]
  ): MyTC[H :: T] = new MyTC[H :: T] {}

  implicit def cnilInstance: MyTC[CNil] = new MyTC[CNil] {}
  implicit def coproductInstance[H, T <: Coproduct](
    implicit
    hInstance: Lazy[MyTC[H]], // wrap in Lazy
    tInstance: MyTC[T]
  ): MyTC[H :+: T] = new MyTC[H :+: T] {}

  implicit def genericInstance[A, R](
    implicit
    generic: Generic.Aux[A, R],
    rInstance: Lazy[MyTC[R]] // wrap in Lazy
  ): MyTC[A] = new MyTC[A] {}

}
