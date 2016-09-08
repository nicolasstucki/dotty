
import Boo._

object Test {
  def main(args: Array[String]): Unit = {
    (new Blinky)(42, boo[Pinky])
    (new Blinky).apply(43, boo[Pinky])

    foo1(44, boo[Pinky])
    foo2(new Blinky)
  }

  def foo1: (Int, Pinky) => Unit = new Blinky
  def foo2(boo: Function2[Int, Pinky, Unit]) = boo(47, boo[Pinky])
}

class Blinky extends Function2[Int, Pinky, Unit] {
  def apply(p1: Int, p2: Pinky) = println("Blinky.apply(" + p1 + ")")
}

object Boo extends Phantom {
  type Pinky <: this.Any
  def boo[B <: this.Any]: B = assume[B]
}
