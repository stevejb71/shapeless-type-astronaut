package play

//noinspection NameBooleanParameters
trait Chapter6 {
  trait Penultimate[L] {
    type Out
    def apply(l: L): Out
  }

  object Penultimate {
    type Aux[L, O] = Penultimate[L] {type Out = O}
    def apply[L](implicit p: Penultimate[L]): Aux[L, p.Out] = p
  }

  // --- New HList op called Penultimate. Compile time safety on length of list and type of element.

  import shapeless._
  import shapeless.ops.hlist
  implicit def hlistPenultimate[L <: HList, Inits <: HList, LastOfInits](
    implicit
    init: hlist.Init.Aux[L, Inits],
    last: hlist.Last.Aux[Inits, LastOfInits]
  ): Penultimate.Aux[L, LastOfInits] = new Penultimate[L] {
      type Out = LastOfInits
      def apply(l: L): LastOfInits = last.apply(init.apply(l))
    }

  implicit class PenultimateOps[A](a: A) {
    def penultimate(implicit p: Penultimate[A]): p.Out = p(a)
  }

  // --- Case class migrations

  case class IceCreamV1(name: String, numCherries: Int, inCone: Boolean)
  case class IceCreamV2a(name: String, inCone: Boolean)
  case class IceCreamV2b(name: String, inCone: Boolean, numCherries: Int)
  case class IceCreamV2c(name: String, inCone: Boolean, numCherries: Int, numWaffles: Int)
  case class IceCreamV2d(inCone: Boolean, name: String)

  // Want to write: IceCreamV1("Sundae", 1, false).migrateTo[IceCreamV2a]

  // Start with a trait for Migration, and a helper for migrateTo syntax.
  trait Migration[A, B] {
    def apply(a: A): B
  }
  implicit class MigrationOps[A](a: A) {
    def migrateTo[B](implicit migration: Migration[A, B]): B = migration(a)
  }

  // Use Intersection op on the two hlists for removal
  // Use Align op for reordering to the same order as B
  // Use Diff to calculate the fields in B not in A
  // Use Prepend to add common fields from both A and B

  implicit def genericMigration[
  A, B, ARepr <: HList, BRepr <: HList,
  Common <: HList, Added <: HList, Unaligned <: HList
  ](
    implicit
    aGen : LabelledGeneric.Aux[A, ARepr],
    bGen : LabelledGeneric.Aux[B, BRepr],
    inter : hlist.Intersection.Aux[ARepr, BRepr, Common],
    diff : hlist.Diff.Aux[BRepr, Common, Added],
    monoid : Monoid[Added],
    prepend : hlist.Prepend.Aux[Added, Common, Unaligned],
    align
    : hlist.Align[Unaligned, BRepr]
  ): Migration[A, B] = (a: A) => bGen.from(align(prepend(monoid.empty, inter(aGen.to(a)))))

  println(IceCreamV1("Sundae", 1, true).migrateTo[IceCreamV2a])  // pure removal
  println(IceCreamV1("Sundae", 1, true).migrateTo[IceCreamV2b])  // pure re-align
  println(IceCreamV1("Sundae", 1, true).migrateTo[IceCreamV2d])  // removal + re-align

  // --- Records

  // Need a specific import
  import shapeless.record._

  val sundae = LabelledGeneric[IceCreamV1].to(IceCreamV1("Sundae", 1, false))

  // Get or update a LabelledGeneric field by name, using a symbol.
  println(sundae.get('name))
  println(sundae.updateWith('name)("MASSIVE " + _))

  // Convert a labelled generic rep to a standard Scala map.
  println(sundae.toMap)

}

object Chapter6 extends Chapter6

object Chapter6App extends App with Chapter6