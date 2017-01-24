import dotty.phantom.PhantomAny

class Pinky extends PhantomAny

object Test {
  def main(args: Array[String]): Unit = {
    (new Blinky2)(new Pinky, new Pinky)
    bar1(new Pinky, new Pinky)
  }
  val bar1: (Pinky, Pinky) => Unit = new Blinky2
}

class Blinky2 extends PhantomsFunction2_00[Pinky, Pinky, Unit] {
  def apply(p1: Pinky, p2: Pinky) = println("Blinky2.apply()")
}
