
/**
  * The Scala implementation of a type class has three parts:
  * • the type class itself, a generic trait;
  * • instances for each type we care about; and
  * • one or more generic interface methods.
  */

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

// The INTERFACE object - this is the object you use
object Printable {
  def format[A](value: A)(implicit printable: Printable[A]): String = printable.format(value)

  def print[A](value: A)(implicit printable: Printable[A]): Unit = println(format(value))
}

// INTERFACE object
object PrintableSyntax {

  implicit class PrintOps[A](value: A) {
    def format(implicit printable: Printable[A]): String = printable.format(value)

    def print(implicit printable: Printable[A]): Unit = println(format)
  }

}

final case class Cat(name: String, age: Int, colour: String)

object MainApp extends App {

  import PrintableInstances._
  import PrintableSyntax._

  val cat = Cat("Smudge", 7, "Black")

  Printable.print(cat)

  Cat("Smudge", 7, "Black").print
}