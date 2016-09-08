import dotty.phantom.PhantomAny

trait Blinky extends PhantomAny
abstract class Inky extends Blinky
class Pinky extends Inky

object Test {
  def main(args: Array[String]): Unit = {
    (new Blinky1)(new Pinky)
    (new Blinky2)(new Pinky, new Pinky)

    val annonPhantomsFunction1 = new PhantomsFunction1[Blinky, Unit] {
      def apply(p1: Blinky) = println("AnnonPhantomsFunction1.apply() 1")
    }
    annonPhantomsFunction1(new Pinky)

    (new PhantomsFunction1[Blinky, Unit] {
      def apply(p1: Blinky) = println("AnnonPhantomsFunction1.apply() 2")
    }).apply(new Pinky)

    foo1(new Pinky)
    foo2(new Pinky)
    foo3(new Pinky)
    foo4(new Blinky1)

    bar1(new Pinky)
    bar2(new Pinky)
    bar3(new Pinky)
  }

  def foo1: Blinky => Unit = new Blinky1
  def foo2: Blinky => Unit = b => println("foo2")
  def foo3 = (b: Blinky) => println("foo3")
  def foo4(boo: PhantomsFunction1[Blinky, Unit]) = boo(new Pinky)

  val bar1: Blinky => Unit = new Blinky1
  val bar2: Blinky => Unit = b => println("bar2")
  val bar3 = (b: Blinky) => println("bar3")
}

class Blinky1 extends PhantomsFunction1[Blinky, Unit] {
  def apply(p1: Blinky) = println("Blinky1.apply()")
}

class Blinky2 extends PhantomsFunction2[Blinky, Pinky, Unit] {
  def apply(p1: Blinky, p2: Pinky) = println("Blinky2.apply()")
}

class Foo extends Function0[Unit] {
  override def apply(): Unit = ()
}
