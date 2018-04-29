package play

//noinspection TypeAnnotation,NameBooleanParameters
trait Chapter5 {
  // -- Singleton types

  import shapeless.syntax.singleton._
  // narrow gives x the singleton type 3
  var x = 3.narrow
//   x = 4  // won't compile

  // -- Type tagging
  import shapeless.{HNil, Witness}
  import shapeless.labelled.{FieldType, field}
  trait Cherries
  val someNumber = 123
  val numCherries1 = "numCherries" ->> someNumber
  val numCherries2: FieldType[Cherries, Int] = field[Cherries](123)

  // Using a Witness to get the field name from a tagged value.

  def getFieldName[K, V](value: FieldType[K, V])(implicit witness: Witness.Aux[K]): K = witness.value
  println(getFieldName(numCherries1))

  def getFieldValue[K, V](value: FieldType[K, V]): V = value
  println(getFieldValue(numCherries1))

  // Shapeless records - HLists of tagged values
  val garfield = ("cat" ->> "Garfield") :: ("orange" ->> true) :: HNil
  println(garfield)

  // -- JSON encoding using LabelledGeneric

  // JSON ADT
  sealed trait JsonValue
  case class JsonObject(fields: List[(String, JsonValue)]) extends JsonValue
  case class JsonArray(items: List[JsonValue]) extends JsonValue
  case class JsonString(value: String) extends JsonValue
  case class JsonNumber(value: Double) extends JsonValue
  case class JsonBoolean(value: Boolean) extends JsonValue
  case object JsonNull extends JsonValue

  // Typeclass for encoder
  trait JsonEncoder[A] {
    def encode(value: A): JsonValue
  }
  object JsonEncoder {
    def apply[A](implicit enc: JsonEncoder[A]): JsonEncoder[A] = enc
  }

  // Typeclass instances
  implicit val stringEncoder: JsonEncoder[String] = JsonString(_)
  implicit val doubleEncoder: JsonEncoder[Double] = JsonNumber(_)
  implicit val intEncoder: JsonEncoder[Int] = JsonNumber(_)
  implicit val booleanEncoder: JsonEncoder[Boolean] = JsonBoolean(_)
  implicit def listEncoder[A](implicit enc: JsonEncoder[A]): JsonEncoder[List[A]] =
    list => JsonArray(list map enc.encode)
  implicit def optionEncoder[A]  (implicit enc: JsonEncoder[A]): JsonEncoder[Option[A]] =
    opt => (opt map enc.encode) getOrElse JsonNull

  case class IceCream(name: String, numCherries: Int, inCone: Boolean)
  val iceCream = IceCream("Sundae", 1, false)

  // Typeclass for JsonObject encoder (need the specific return value in hlistEncoder below).
  trait JsonObjectEncoder[A] {
    def encode(value: A): JsonObject
  }

  import shapeless.{HList, Lazy, ::}
  implicit val hnilEncoder: JsonObjectEncoder[HNil] = _ => JsonObject(Nil)
  implicit def hlistEncoder[K <: Symbol, H, T <: HList](
    implicit
      witness: Witness.Aux[K],
      hEncoder: Lazy[JsonEncoder[H]],
      tEncoder: JsonObjectEncoder[T]
    ): JsonObjectEncoder[FieldType[K, H] :: T] = {
    val fieldName = witness.value.name
    hlist => {
      val head = hEncoder.value.encode(hlist.head)
      val tail = tEncoder.encode(hlist.tail)
      JsonObject((fieldName, head) :: tail.fields)
    }
  }

  import shapeless.LabelledGeneric
  implicit def genericObjectEncoder[A, H](
    implicit
      generic: LabelledGeneric.Aux[A, H],
      hEncoder: Lazy[JsonObjectEncoder[H]]
  ): JsonEncoder[A] = x => hEncoder.value.encode(generic.to(x))

  println(JsonEncoder[IceCream].encode(iceCream))

  // -- LabelledGeneric for coproducts
  sealed trait Shape
  case class Rectangle(width: Double, height: Double) extends Shape
  case class Circle(radius: Double) extends Shape

  println(LabelledGeneric[Shape].to(Circle(1.0)))
}

object Chapter5 extends Chapter5

object Chapter5App extends App with Chapter5