
import Phantoms._

object Test {
  def main(args: Array[String]): Unit = {
    foo3(44, Boo.boo[Pinky], 0.4, Boo.boo[Pinky], Boo.boo[Pinky])
  }

  val foo3: (Int, Pinky, Double, Pinky, Pinky) => Unit = new Blinky2().asInstanceOf[Function5[Int, Pinky, Double, Pinky, Pinky, Unit]]
}

class Blinky2 extends Blinky

trait Blinky extends Function5[Int, Pinky, Double, Pinky, Pinky, Unit] {
  def apply(p1: Int, p2: Pinky, p3: Double, p4: Pinky, p5: Pinky) = println("Blinky.apply(" + p1 + ")")
}

object Phantoms extends Phantoms
trait Phantoms {
  type Pinky <: Boo.Any
}

object Boo extends Phantom {
  def boo[B <: Boo.Any]: B = assume[B]
}
