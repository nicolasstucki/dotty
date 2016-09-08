
import Phantoms._

object Test {
  def main(args: Array[String]): Unit = {
    (new Blinky1)(42)
    foo1(42)
    foo2(new Blinky1)
  }

  def foo1: Int => Pinky = new Blinky1
  def foo2(boo: Function1[Int, Pinky]) = boo(42)
}

class Blinky1 extends Function1[Int, Pinky] {
  def apply(i: Int) = {
    println("Blinky1.apply()")
    Boo.boo[Pinky]
  }
}

object Phantoms extends Phantoms
trait Phantoms {
  type Pinky <: Boo.Any
}

object Boo extends Phantom {
  def boo[B <: Boo.Any]: B = assume[B]
}
