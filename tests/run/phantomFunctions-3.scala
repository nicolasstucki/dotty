import dotty.phantom.PhantomAny

class Pinky extends PhantomAny

object Test {
  def main(args: Array[String]): Unit = {
    foo3(44, new Pinky, 0.4, new Pinky, new Pinky)
  }

  def foo3: (Int, Pinky, Double, Pinky, Pinky) => Unit = new Blinky2
}

class Blinky2 extends Function5[Int, Pinky, Double, Pinky, Pinky, Unit] {
  def apply(p1: Int, p2: Pinky, p3: Double, p4: Pinky, p5: Pinky) = println("Blinky.apply(" + p1 + ")")
}
