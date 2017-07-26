
/**
  * 1.1 - Anatomy of a Type Class
  *
  * The Scala implementation of a type class has three parts:
  * • the type class itself, a generic trait;
  * • instances for each type we care about; and
  * • one or more generic interface methods.
  */

object AnatomyOfATypeClass extends App {

  // The TYPE CLASS itself
  trait Printable[A] {
    def format(value: A): String
  }

  // INSTANCES of Printable
  object PrintableInstances {
    implicit val stringPrintable = new Printable[String] {
      def format(value: String): String = value
    }
    implicit val intPrintable = new Printable[Int] {
      def format(value: Int): String = value.toString
    }
    implicit val catPrintable = new Printable[Cat] {
      def format(cat: Cat): String = {
        val name = Printable.format(cat.name)
        val age = Printable.format(cat.age)
        val colour = Printable.format(cat.colour)
        s"$name is a $age year-old $colour cat."
      }
    }
  }

  /**
    * Interface methods can be defined in interface objects or interface syntax.
    * Implicit classes are the most common way of implementing syntax.
    */

  // The INTERFACE object - this is the object you use
  object Printable {
    def format[A](value: A)(implicit printable: Printable[A]): String = printable.format(value)

    def print[A](value: A)(implicit printable: Printable[A]): Unit = println(format(value))
  }

  // INTERFACE syntax
  object PrintableSyntax {

    implicit class PrintOps[A](value: A) {
      def format(implicit printable: Printable[A]): String = printable.format(value)

      def print(implicit printable: Printable[A]): Unit = println(format)
    }

  }

  final case class Cat(name: String, age: Int, colour: String)

  import PrintableInstances._
  import PrintableSyntax._

  val cat = Cat("Smudge", 7, "Black")

  Printable.print(cat)

  Cat("Smudge", 7, "Black").print
}

/**
  * 1.2 - Meet Cats
  *
  * Cats type classes are defined in the cats package. For example, the
  * Show type class is defined as cats.Show .
  *
  * Default instances are defined in the cats.instances package. Imports
  * are organized by parameter type (as opposed to by type class).
  *
  * Interface syntax is defined in the cats.syntax package. There are separate
  * syntax imports for each type class. For example, the syntax for
  * Show is defined in cats.syntax.show .
  */

object MeetCats extends App {

  import cats.Show
  import cats.instances.int._
  import cats.instances.string._
  import cats.syntax.show._

  final case class Cat(name: String, age: Int, colour: String)

  val cat = Cat("Smudge", 7, "Black")

  implicit val catShow: Show[Cat] = Show.show(cat => s"${cat.name.show} is a ${cat.age.show} year-old ${cat.colour.show} cat.")

  println(cat.show)
}