import shapeless._
import syntax.std.function._
import ops.function._

case class Person(name: String, age: Int)

val gen = Generic[Person]

val fn = (n: String, a: Int) => s"Name: $n, Age: $a"
val ft: ((String, Int)) => String = fn.tupled

val fntp = FnToProduct[(String, Int) => String]

// FnToProduct.Aux[(String, Int) => String, (::[String, ::[Int, HNil]]) => String]#Out
val fn1: String :: Int :: HNil => String = fntp(fn)
val fn2: String :: Int :: HNil => String = fn.toProduct

val person = Person("Gabi", 28)

val a: String :: Int :: HNil => String = fn.toProduct
val b: String :: Int :: HNil = gen.to(person)
val c: String = a(b)
val c2: String = fn.toProduct(gen.to(person))

val hl = 28 :: "Gabi" :: 8.7 :: HNil
val t: (Int, String, Double) = hl.tupled

val personAsHL = "Gabi" :: 28 :: HNil
val gabi: Person = gen.from(personAsHL)