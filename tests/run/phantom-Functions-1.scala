
import Boo._

object Test {
  def main(args: Array[String]): Unit = {
    (new Blinky2)(boo[Pinky], boo[Pinky])
    bar1(boo[Pinky], boo[Pinky])
  }
  val bar1: (Pinky, Pinky) => Unit = new Blinky2

}

class Blinky2 extends Function2[Pinky, Pinky, Unit] {
  def apply(p1: Pinky, p2: Pinky) = println("Blinky2.apply()")
}

object Boo extends Phantom {
  type Pinky <: this.Any
  def boo[B <: this.Any]: B = assume[B]
}
