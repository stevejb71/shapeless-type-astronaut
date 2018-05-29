package play

//noinspection NameBooleanParameters
trait Chapter7 {
  import shapeless._

  // Polymorphic function handling multiple input types
  object myPoly extends Poly1 {
    implicit val intCase: Case.Aux[Int, Double] = at(_ / 2.0)
    implicit val stringCase: Case.Aux[String, Double] = at(_.length)
  }
  println(s"""${myPoly(123)} and ${myPoly("abc")}""")

  // Totalling numbers generically (using Numeric as a typeclass)
  object total extends Poly1 {
    implicit def base[A](implicit num: Numeric[A]): Case.Aux[A, Double] = at(num.toDouble)
    implicit def option[A](implicit num: Numeric[A]): Case.Aux[Option[A], Double] = at(opt => opt.map(num.toDouble).getOrElse(0.0))
    implicit def list[A](implicit num: Numeric[A]): Case.Aux[List[A], Double] = at(list => num.toDouble(list.sum))
  }
  println(s"""${total(10)} and ${total(Option(8))} and ${total(List(8, 1, 4))}""")

  // Sizeof mapping over HLists
  // Removing a case from sizeOf will produce compile error.
  // flatMap also available if cases return HLists
  object sizeOf extends Poly1 {
    implicit val intCase: Case.Aux[Int, Int] = at(identity)
    implicit val stringCase: Case.Aux[String, Int] = at(_.length)
    implicit val booleanCase: Case.Aux[Boolean, Int] = at(bool => if(bool) 1 else 0)
  }
  println((10 :: "hello" :: true :: "xyz" :: HNil).map(sizeOf))

  // Folding
  object sum extends Poly2 {
    implicit val intIntCase: Case.Aux[Int, Int, Int] = at((a, b) => a + b)
    implicit val intStringCase: Case.Aux[Int, String, Int] = at((a, b) => a + b.length)
    implicit val intBoolCase: Case.Aux[Int, Boolean, Int] = at((a, b) => a + (if (b) 1 else 0))
  }
  println((10 :: "hello" :: true :: "xyz" :: HNil).foldLeft(0)(sum))

  // -- Typeclasses using Poly

  // Mapping from one case class to another
  trait ProductMapper[A, B, P] {
    def apply(a: A): B
  }

  import shapeless.ops.hlist

  // Generic mapper from one case class A to B via mapper
  implicit def genericProductMapper[A, B, P <: Poly, ARepr <: HList, BRepr <: HList](
    implicit
    aGen: Generic.Aux[A, ARepr],
    bGen: Generic.Aux[B, BRepr],
    mapper: hlist.Mapper.Aux[P, ARepr, BRepr]
  ): ProductMapper[A, B, P] = (a: A) => bGen.from(mapper(aGen.to(a)))

  // Syntax
  implicit class ProductMapperOps[A](a: A) {
    class Builder[B] {
      def apply[P <: Poly](poly: P)(implicit pm: ProductMapper[A, B, P]): B = pm(a)
    }
    def mapTo[B]: Builder[B] = new Builder[B]
  }

  // Usage
  object conversions extends Poly1 {
    implicit val intCase: Case.Aux[Int, Boolean] = at(_ > 0)
    implicit val boolCase: Case.Aux[Boolean, Int] = at(if(_) 1 else 0)
    implicit val strCase: Case.Aux[String, String] = at("Cold " + _)
  }
  case class IceCream1(name: String, numCherries: Int, inCone: Boolean)
  case class IceCream2(name: String, hasCherries: Boolean, numCones: Int)

  println(IceCream1("Sundae", 1, false).mapTo[IceCream2](conversions))
}

object Chapter7 extends Chapter7

object Chapter7App extends App with Chapter7