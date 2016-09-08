
import Phantoms._

object Test {
  def main(args: Array[String]): Unit = {
    (new Blinky1)(Boo.boo[Pinky])
    foo1(Boo.boo[Pinky])
    foo2(new Blinky1)
  }

  def foo1: Pinky => Pinky = new Blinky1
  def foo2(boo: Function1[Pinky, Pinky]) = boo(Boo.boo[Pinky])
}

class Blinky1 extends Function1[Pinky, Pinky] {
  def apply(p1: Pinky) = {
    println("Blinky1.apply()")
    p1
  }
}

object Phantoms extends Phantoms
trait Phantoms {
  type Pinky <: Boo.Any
}

object Boo extends Phantom {
  def boo[B <: Boo.Any]: B = assume[B]
}
