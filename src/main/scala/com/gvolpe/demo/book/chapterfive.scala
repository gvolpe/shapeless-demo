package com.gvolpe.demo.book

import com.gvolpe.demo.book.chaptertwo.{Circle, Rectangle, Shape}
import shapeless._
import labelled.{field, KeyTag, FieldType}
import syntax.singleton._

object chapterfive {

  // Literal Types
  //var x: Int(42) = 42.narrow
  // x = 43 // This does not compile

  //var y: 42 = 42 // Only works with the compiler flag -Yliteral-types in Typelvel Scala

  // Tagging and Phantom Types
  trait Cherries

  val number = 42
  val numCherries = number.asInstanceOf[Int with Cherries]

  val someNumber = 123
  val numCherriesKeyTag = "numCherries" ->> someNumber  // Int with KeyTag[String("numCherries"),Int] = 123

  // type FieldType[K, V] = V with KeyTag[K, V]
  field[Cherries](123) // FieldType[Cherries,Int] = 123

  // Get the tag from a tagged value:
  def getFieldName[K, V](value: FieldType[K, V])
      (implicit witness: Witness.Aux[K]): K = witness.value

  //getFieldName(numCherries) // String = numCherries

  // Get the untagged type of a tagged value:
  def getFieldValue[K, V](value: FieldType[K, V]): V = value

  //getFieldValue(numCherries) // Int = 123

  // Records are HLists of tagged elements
  val garfield = ("cat" ->> "Garfield") :: ("orange" ->> true) :: HNil
  // garfield: FieldType["cat", String] :: FieldType["orange", Boolean] ::  HNil

  // Deriving product instances using LabelledGeneric
  sealed trait JsonValue
  case class JsonObject(fields: List[(String, JsonValue)]) extends JsonValue
  case class JsonArray(items: List[JsonValue]) extends JsonValue
  case class JsonString(value: String) extends JsonValue
  case class JsonNumber(value: Double) extends JsonValue
  case class JsonBoolean(value: Boolean) extends JsonValue
  case object JsonNull extends JsonValue

  trait JsonEncoder[A] {
    def encode(value: A): JsonValue
  }

  object JsonEncoder {
    def apply[A](implicit enc: JsonEncoder[A]): JsonEncoder[A] = enc
  }

  def createEncoder[A](func: A => JsonValue): JsonEncoder[A] =
    new JsonEncoder[A] {
      def encode(value: A): JsonValue = func(value)
    }

  implicit val stringEncoder: JsonEncoder[String] = createEncoder(str => JsonString(str))
  implicit val doubleEncoder: JsonEncoder[Double] = createEncoder(num => JsonNumber(num))
  implicit val intEncoder: JsonEncoder[Int] = createEncoder(num => JsonNumber(num))
  implicit val booleanEncoder: JsonEncoder[Boolean] = createEncoder(bool => JsonBoolean(bool))

  implicit def listEncoder[A]
      (implicit enc: JsonEncoder[A]): JsonEncoder[List[A]] =
    createEncoder(list => JsonArray(list.map(enc.encode)))

  implicit def optionEncoder[A]
      (implicit enc: JsonEncoder[A]): JsonEncoder[Option[A]] =
    createEncoder(opt => opt.map(enc.encode).getOrElse(JsonNull))

  // Example of what we want to get ideally
  case class IceCream(name: String, numCherries: Int, inCone: Boolean)
  val iceCream = IceCream("Sundae", 1, false)

  val iceCreamJson: JsonValue =
    JsonObject(List(
      "name"        -> JsonString("Sundae"),
      "numCherries" -> JsonNumber(1),
      "inCone"      -> JsonBoolean(false)
    ))

  // LabelledGeneric instance
  val gen = LabelledGeneric[IceCream].to(iceCream)
  // String with KeyTag[Symbol with Tagged["name"], String]     ::
  // Int with KeyTag[Symbol with Tagged["numCherries"], Int]    ::
  // Boolean with KeyTag[Symbol with Tagged["inCone"], Boolean] ::
  // HNil

  // HLists instance
  trait JsonObjectEncoder[A] extends JsonEncoder[A] {
    def encode(value: A): JsonObject
  }

  def createObjectEncoder[A](fn: A => JsonObject): JsonObjectEncoder[A] =
    new JsonObjectEncoder[A] {
      def encode(value: A): JsonObject = fn(value)
    }

  implicit val hnilEncoder: JsonObjectEncoder[HNil] =
    createObjectEncoder(hnil => JsonObject(Nil))

  implicit def hlistObjectEncoder[K <: Symbol, H, T <: HList](
    implicit
    witness: Witness.Aux[K],
    hEncoder: Lazy[JsonEncoder[H]],
    tEncoder: JsonObjectEncoder[T]
  ): JsonObjectEncoder[FieldType[K, H] :: T] = {
    val fieldName: String = witness.value.name // value has type Symbol so we need to invoke name afterwards
    createObjectEncoder { hlist =>
      val head = hEncoder.value.encode(hlist.head)
      val tail = tEncoder.encode(hlist.tail)
      JsonObject((fieldName, head) :: tail.fields)
    }
  }

  // Generic encoder
  implicit def genericObjectEncoder[A, H](
    implicit
    generic: LabelledGeneric.Aux[A, H],
    hEncoder: Lazy[JsonObjectEncoder[H]]
  ): JsonEncoder[A] =
    createObjectEncoder { value =>
      hEncoder.value.encode(generic.to(value))
    }

  val json = JsonEncoder[IceCream].encode(iceCream)

  // Deriving CoProducts with LabelledGeneric
  val labelledCoproduct = LabelledGeneric[Shape].to(Circle(1.0))
  // Rectangle with KeyTag[Symbol with Tagged["Rectangle"], Rectangle] :+:
  // Circle with KeyTag[Symbol with Tagged["Circle"], Circle] :+:
  // CNil

  implicit val cnilObjectEncoder: JsonObjectEncoder[CNil] =
    createObjectEncoder(cnil => throw new Exception("Inconceivable!"))

  implicit def coproductObjectEncoder[K <: Symbol, H, T <: Coproduct](
    implicit
    witness: Witness.Aux[K],
    hEncoder: Lazy[JsonEncoder[H]],
    tEncoder: JsonObjectEncoder[T]
  ): JsonObjectEncoder[FieldType[K, H] :+: T] = {
    val typeName = witness.value.name
    createObjectEncoder {
      case Inl(h) =>
        JsonObject(List(typeName -> hEncoder.value.encode(h)))
      case Inr(t) =>
        tEncoder.encode(t)
    }
  }

  val shape: Shape = Circle(1.0)
  val jsonShape = JsonEncoder[Shape].encode(shape)

}
