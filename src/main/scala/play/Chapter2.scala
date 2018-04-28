package play

//noinspection NameBooleanParameters
trait Chapter2 {
  import shapeless.Generic
  import shapeless.{::, HNil}
  import shapeless.{:+:, CNil, Inl, Inr}

  case class IceCream(name: String, numCherries: Int, inCone: Boolean)
  case class Employee(name: String, number: Int, manager: Boolean)

  // --- Generic Product mapping via HList

  val iceCream = IceCream("Sundae", 1, false)

  val iceCreamRep: String :: Int :: Boolean :: HNil = Generic[IceCream].to(iceCream)

  val employee: Employee = Generic[Employee].from(iceCreamRep)
  println(employee)

  // -- Coproduct from unrelated classes (no inheritance)

  case class Red()
  case class Amber()
  case class Green()

  type Light = Red :+: Amber :+: Green :+: CNil

  val amber: Light = Inr(Inl(Amber()))
  println(amber)

  // -- Coproduct from sealed trait + case classes

  sealed trait Shape
  case class Rectangle(width: Double, height: Double) extends Shape
  case class Circle(radius: Double) extends Shape

  val rectangle = Rectangle(10.0, 12.0)

  val rectangleRep: Circle :+: Rectangle :+: CNil = Generic[Shape].to(rectangle)
  println(rectangleRep)
}

object Chapter2 extends Chapter2

// Chapter2 as Object, extending App, makes it unusable in the repl (some DelayedInit thing perhaps).
object Chapter2App extends App with Chapter2 {

}
