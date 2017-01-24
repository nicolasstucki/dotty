import dotty.phantom.PhantomAny

class Pinky extends PhantomAny

object Test {
  def main(args: Array[String]): Unit = {
    (new Blinky1)(new Pinky)
    foo1(new Pinky)
    foo2(new Blinky1)
  }

  def foo1: Pinky => Unit = new Blinky1
  def foo2(boo: PhantomsFunction1_0[Pinky, Unit]) = boo(new Pinky)
}

class Blinky1 extends PhantomsFunction1_0[Pinky, Unit] {
  def apply(p1: Pinky) = println("Blinky1.apply()")
}
