package play

import shapeless._
import shapeless.labelled._

trait Monoid[A] {
  def empty: A
  def combine(a1: A, a2: A): A
}

//noinspection TypeAnnotation
object Monoid {
  def createMonoid[A](zero: A)(add: (A, A) => A): Monoid[A] = new Monoid[A] {
      def empty = zero
      def combine(x: A, y: A): A = add(x, y)
    }
  implicit val stringMonoid = createMonoid("")(_ + _)

  implicit val hnilMonoid: Monoid[HNil] = createMonoid[HNil](HNil)((_, _) => HNil)
  implicit def hlistMonoid[K <: Symbol, H, T <: HList](
    implicit
    hMonoid: Lazy[Monoid[H]],
    tMonoid: Monoid[T]
  ): Monoid[FieldType[K, H] :: T] =
    createMonoid(field[K](hMonoid.value.empty) :: tMonoid.empty) {
      (x, y) =>
        field[K](hMonoid.value.combine(x.head, y.head)) ::
          tMonoid.combine(x.tail, y.tail)
    }
}

object MonoidApp extends App {
  def printEmpty[M](implicit m: Monoid[M]) = println(s"'${m.empty}'")
  printEmpty[String]
  printEmpty[HNil]
  printEmpty[FieldType[Symbol, String] :: HNil]
}
