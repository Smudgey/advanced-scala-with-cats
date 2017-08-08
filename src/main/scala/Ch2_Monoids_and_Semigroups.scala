
trait Semigroup[A] {
  def combine(x: A, y: A): A
}

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

object Monoid {
  def apply[A](implicit monoid: Monoid[A]): Monoid[A] = monoid
}

object Laws {
  //Monoid and Semigroup
  def associativeLaw[A](x: A, y: A, z: A)(implicit m: Monoid[A]): Boolean =
    m.combine(x, m.combine(y, z)) == m.combine(m.combine(x, y), z)

  //Monoid
  def identityLaw[A](x: A)(implicit m: Monoid[A]): Boolean =
    (m.combine(x, m.empty) == x) && (m.combine(m.empty, x) == x)
}

/**
  * 2.3 The Truth About Monoids
  */
object TheTruthAboutMonoids extends App {

  import Laws._

  implicit val booleanAndMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    override def combine(x: Boolean, y: Boolean): Boolean = x && y

    override def empty: Boolean = true
  }

  implicit val booleanOrMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    override def combine(x: Boolean, y: Boolean): Boolean = x || y

    override def empty: Boolean = false
  }

  implicit val booleanEitherMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    override def combine(x: Boolean, y: Boolean): Boolean = (x && !y) || (!x && y)

    override def empty: Boolean = false
  }

  implicit val booleanXnorMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    override def combine(x: Boolean, y: Boolean): Boolean = (x || !y) && (!x || y)

    override def empty: Boolean = true
  }

  println("Associative Law Checks")
  println("booleanAndMonoid (true, false, true): " + associativeLaw(true, false, true)(booleanAndMonoid))
  println("booleanAndMonoid (false, true, false): " + associativeLaw(false, true, false)(booleanAndMonoid))
  println("\nbooleanOrMonoid (true, false, true): " + associativeLaw(true, false, true)(booleanOrMonoid))
  println("booleanOrMonoid (false, true, false): " + associativeLaw(false, true, false)(booleanOrMonoid))
  println("\nbooleanEitherMonoid (true, false, true): " + associativeLaw(true, false, true)(booleanEitherMonoid))
  println("booleanEitherMonoid (false, true, false): " + associativeLaw(false, true, false)(booleanEitherMonoid))
  println("\nbooleanXnorMonoid (true, false, true): " + associativeLaw(true, false, true)(booleanXnorMonoid))
  println("booleanXnorMonoid (false, true, false): " + associativeLaw(false, true, false)(booleanXnorMonoid))

  println("\nIdentity Law Checks")
  println("booleanAndMonoid (true): " + identityLaw(true)(booleanAndMonoid))
  println("booleanAndMonoid (false): " + identityLaw(false)(booleanAndMonoid))
  println("\nbooleanOrMonoid (true): " + identityLaw(true)(booleanOrMonoid))
  println("booleanOrMonoid (false): " + identityLaw(false)(booleanOrMonoid))
  println("\nbooleanEitherMonoid (true): " + identityLaw(true)(booleanEitherMonoid))
  println("booleanEitherMonoid (false): " + identityLaw(false)(booleanEitherMonoid))
  println("\nbooleanXnorMonoid (true): " + identityLaw(true)(booleanXnorMonoid))
  println("booleanXnorMonoid (false): " + identityLaw(false)(booleanXnorMonoid))
}

/**
  * 2.4 All Set for Monoids
  *
  * Doing implicit defs allows the to use generic types
  *
  * Set add and Union are monoids. Intersect fails the
  * Identity Law check, thus it is a Semigroup
  */
object AllSetForMonoids extends App {

  import Laws._

  implicit def setAddMonoid[A](): Monoid[Set[A]] = new Monoid[Set[A]] {
    override def combine(x: Set[A], y: Set[A]): Set[A] = x ++ y

    override def empty: Set[A] = Set.empty[A]
  }

  implicit val setMultiplySemigroup: Semigroup[Set[Int]] = (x: Set[Int], y: Set[Int]) => x zip y map { case (a, b) => a * b }

  implicit def setUnionMonoid[A](): Monoid[Set[A]] = new Monoid[Set[A]] {
    override def combine(x: Set[A], y: Set[A]): Set[A] = x union y

    override def empty: Set[A] = Set.empty[A]
  }

  implicit def setIntersectMonoid[A](): Monoid[Set[A]] = new Monoid[Set[A]] {
    override def combine(x: Set[A], y: Set[A]): Set[A] = x intersect y

    override def empty: Set[A] = Set.empty[A]
  }

  val set1 = Set(1, 2, 3)
  val set2 = Set(5, 10, 15)
  val set3 = Set(10, 20, 30)

  println("Associative Law Checks")
  println("setAddMonoid (set1, set2, set3): " + associativeLaw(set1, set2, set3)(setAddMonoid()))
  println("setUnionMonoid (set1, set2, set3): " + associativeLaw(set1, set2, set3)(setUnionMonoid()))
  println("setIntersectMonoid (set1, set2, set3): " + associativeLaw(set1, set2, set3)(setIntersectMonoid()))

  println("\nIdentity Law Checks")
  println("setAddMonoid (set1): " + identityLaw(set1)(setAddMonoid()))
  println("setUnionMonoid (set1): " + identityLaw(set1)(setUnionMonoid()))
  println("setIntersectMonoid (set1): " + identityLaw(set1)(setIntersectMonoid()))
}
