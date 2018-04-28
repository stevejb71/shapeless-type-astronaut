package play

//noinspection NameBooleanParameters
trait Chapter3 {
  // --- Typeclass derivation

  import shapeless.{HList, ::, HNil, the, Generic, Lazy}

  trait CsvEncoder[A] {
    def encode(value: A): List[String]
  }
  def writeCsvs[A](xs: Seq[A])(implicit csvEncoder: CsvEncoder[A]): String =
    xs.map(x => csvEncoder.encode(x).mkString(",")).mkString("\n")

  // Example case class
  case class IceCream(name: String, numCherries: Int, inCone: Boolean)
  val iceCream = IceCream("Sundae", 12, true)
  val iceCreams: List[IceCream] = List(
    IceCream("Sundae", 1, false),
    IceCream("Cornetto", 0, true),
    IceCream("Banana Split", 0, false)
  )

  // Typeclass interface
  // Primitive typeclass instances
  implicit val stringEncoder: CsvEncoder[String] = x => List(x)
  implicit val intEncoder: CsvEncoder[Int] = x => List(x.toString)
  implicit val booleanEncoder: CsvEncoder[Boolean] = x => List(if(x) "yes" else "no")
  implicit val doubleEncoder: CsvEncoder[Double] = x => List(x.toString)

  // HList typeclass instances
  implicit val hnilEncoder: CsvEncoder[HNil] = Function.const(Nil)
  implicit def hlistEncoder[H, T <: HList](implicit hEncoder: Lazy[CsvEncoder[H]], tEncoder: CsvEncoder[T]): CsvEncoder[H :: T] = {
    case h :: t => hEncoder.value.encode(h) ++ tEncoder.encode(t)
  }

  // IceCream specific encoder using Generic

  // Now we can create an instance for the representation of IceCream
  // Use 'the' which summons a typeclass instance. implicitly doesn't always infer the right type.
  object IceCreamDerivation {
    val iceCreamRepEncoder: CsvEncoder[String :: Int :: Boolean :: HNil] = the[CsvEncoder[String :: Int :: Boolean :: HNil]]
    println(iceCreamRepEncoder.encode(Generic[IceCream].to(iceCream)))

    implicit val iceCreamEncoder: CsvEncoder[IceCream] = {
      val gen = Generic[IceCream]
      val enc = the[CsvEncoder[gen.Repr]]
      iceCream => enc.encode(gen.to(iceCream))
    }
  }

  // Generic encoder for any product type

  // A is the type (IceCream), R is the rep (HList)
  implicit def genericEncoder[A, R](
    implicit
      gen: Generic.Aux[A, R], // alias of Generic[A] {type Repr = R}
      enc: Lazy[CsvEncoder[R]]
  ): CsvEncoder[A] = a => enc.value.encode(gen.to(a))

  println(implicitly[CsvEncoder[IceCream]].encode(iceCream))
  println(writeCsvs(iceCreams))

  // Generic encoder for coproduct types

  import shapeless.{Coproduct, :+:, CNil, Inl, Inr}

  sealed trait Shape
  case class Rectangle(width: Double, height: Double) extends
    Shape
  case class Circle(radius: Double) extends Shape
  val shapes: List[Shape] = List(Rectangle(3.0, 4.0), Circle(1.0))

  implicit val cnilEncoder: CsvEncoder[CNil] = _ => throw new Exception("unreachable")
  implicit def coproductEncoder[H, T <: Coproduct](
    implicit
     hEncoder: Lazy[CsvEncoder[H]],
     tEncoder: CsvEncoder[T]
    ): CsvEncoder[H :+: T] = {
    case Inl(h) => hEncoder.value.encode(h)
    case Inr(t) => tEncoder.encode(t)
  }

  println(writeCsvs(shapes))

  // Generic encoder for recursive types
  // Requires use of Lazy to break the recursive implicit search
  sealed trait Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  case class Leaf[A](value: A) extends Tree[A]
  val tree = Branch(Branch(Leaf(4), Branch(Leaf(3), Leaf(5))), Branch(Leaf(6), Leaf(7)))

  println(writeCsvs(List(tree)))

  // -- Debugging

  // Can use reify to get an AST of a typeclass instance at runtime.
  // Nothing or Any are evidence of failure.
  import scala.reflect.runtime.universe._
  println(reify(implicitly[CsvEncoder[Tree[String]]]))
}


object Chapter3 extends Chapter3

// Chapter3 as Object, extending App, makes it unusable in the repl (some DelayedInit thing perhaps).
object Chapter3App extends App with Chapter3