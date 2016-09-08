
import Boo._

object Test {

  def main(args: Array[String]): Unit = {
    (new Blinky1)(boo[Pinky])
    foo1(boo[Pinky])
    foo2(new Blinky1)
  }

  def foo1: Pinky => Unit = new Blinky1
  def foo2(boo: Function1[Pinky, Unit]) = boo(boo[Pinky])
}

class Blinky1 extends Function1[Pinky, Unit] {
  def apply(p1: Pinky) = println("Blinky1.apply()")
}

object Boo extends Phantom {
  type Pinky <: Boo.Any
  def boo[B <: Pinky]: B = assume[B]
}
