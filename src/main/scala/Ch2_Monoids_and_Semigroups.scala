
trait Semigroup[A] {
  def combine(x: A, y: A): A
}

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

object Monoid {
  def apply[A](implicit monoid: Monoid[A]): Monoid[A] = monoid
}

/**
  * The Truth About Monoids
  */
object TheTruthAboutMonoids extends App {

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

  def associativeLaw[A](x: A, y: A, z: A)(implicit m: Monoid[A]): Boolean =
    m.combine(x, m.combine(y, z)) == m.combine(m.combine(x, y), z)

  def identityLaw[A](x: A)(implicit m: Monoid[A]): Boolean =
    (m.combine(x, m.empty) == x) && (m.combine(m.empty, x) == x)

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

object AllSetForMonoids extends App {

}
