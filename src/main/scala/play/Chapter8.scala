package play

import shapeless.ops.coproduct.Length.Aux


//noinspection TypeAnnotation
trait Chapter8 {
  // Type level naturals
  import shapeless._
  import shapeless.ops.nat.ToInt

  // Use ToInt to get the runtime representation, or the Nat.toInt shortcut
  println(ToInt[Succ[Nat._12]].apply())
  println(Nat.toInt[Succ[Nat._22]])


  // Getting the compile time lengths of HLists & coproducts
  import shapeless.ops.{hlist, coproduct, nat}
  import shapeless.ops.hlist.Length
  val hlistLength: Length.Aux[String :: Int :: Boolean :: HNil, Nat._3] = hlist.Length[String :: Int :: Boolean :: HNil]
  println(Nat.toInt[hlistLength.Out])

  val coproductLength: Aux[Double :+: Char :+: CNil, Nat._2] = coproduct.Length[Double :+: Char :+: CNil]
  println(Nat.toInt[coproductLength.Out])

  // Getting the compile time length of a product

  // As usual, start with a functional trait
  trait SizeOf[A] {
    def value: Int
  }

  // Get the size of a type via implicit lookup
  def sizeOf[A](implicit size: SizeOf[A]): Int = size.value

  // Implicitly look up a generic, get its length, and convert to int, giving an implicit SizeOf
  implicit def genericSizeOf[A, L <: HList, N <: Nat](
    implicit
      _x: Generic.Aux[A, L],
      _y: hlist.Length.Aux[L, N],
      sizeToInt: nat.ToInt[N]
  ): SizeOf[A] = new SizeOf[A] { val value = sizeToInt() }

  case class IceCream(name: String, numCherries: Int, inCone: Boolean)
  println(sizeOf[IceCream])

  // -- Random case class generator (basis of deriving Arb for quickcheck)

  // The usual stuff
  trait Random[A] {
    def get(): A
  }
  def random[A](implicit r: Random[A]): A = r.get()

  // Randoms for basic types
  implicit val intRandom: Random[Int] = () => scala.util.Random.nextInt(10)
  implicit val charRandom: Random[Char] = () => ('A'.toInt + scala.util.Random.nextInt(26)).toChar
  implicit val booleanRandom: Random[Boolean] = () => scala.util.Random.nextBoolean
  implicit val stringRandom: Random[String] = () => scala.util.Random.nextString(scala.util.Random.nextInt(10))

  // - Random for products
  // Random for generics
  implicit def genericRandom[A, R](
    implicit
      gen: Generic.Aux[A, R],
      random: Lazy[Random[R]]
  ): Random[A] = () => gen.from(random.value.get())
  // Random for HLists
  implicit val hnilRandom: Random[HNil] = () => HNil
  implicit def hlistRandom[H, T <: HList](
    implicit
      hRandom: Lazy[Random[H]],
      tRandom: Random[T]
  ): Random[H :: T] = () => hRandom.value.get :: tRandom.get

  // Now a random IceCream
  for(_ <- 1 to 5) println(random[IceCream])

  // - Random coproducts

  // Need to know the length of the coproduct and then choose based on that.
  // The natural recursive implementation will not choose fairly.
  implicit val cnilRandom: Random[CNil] = () => throw new Exception("Inconceivable")

  implicit def coproductRandom[H, T <: Coproduct, L <: Nat](
  implicit
    hRandom: Lazy[Random[H]],
    tRandom: Random[T],
    tLength: coproduct.Length.Aux[T, L],
    tLengthAsInt: ToInt[L]
  ): Random[H :+: T] = () => {
    val length = 1 + tLengthAsInt()
    val chooseH = scala.util.Random.nextDouble < (1.0 / length)
    if (chooseH) Inl(hRandom.value.get()) else Inr(tRandom.get())
  }

  sealed trait Light
  case object Red extends Light
  case object Amber extends Light
  case object Green extends Light

  for(_ <- 1 to 5) println(random[Light])

  // -- List manipulation functions

  // Take, drop, updateAt, etc, all using Nats
  val hlist2 = 123 :: "foo" :: true :: 'x' :: HNil

  println(hlist2(Nat._2))
  println(hlist2.take(Nat._3).drop(Nat._1))
  println(hlist2.updatedAt(Nat._1, "bar").updatedAt(Nat._2, "baz"))
}

object Chapter8 extends Chapter8

object Chapter8App extends App with Chapter8