import dotty.phantom.PhantomAny

class Pinky extends PhantomAny

object Test {
  def main(args: Array[String]): Unit = {
    (new Blinky)(42, new Pinky)
    (new Blinky).apply(43, new Pinky)

    foo1(44, new Pinky)
    foo2(new Blinky)
  }

  def foo1: (Int, Pinky) => Unit = new Blinky
  def foo2(boo: Function2[Int, Pinky, Unit]) = boo(47, new Pinky)
}

class Blinky extends Function2[Int, Pinky, Unit] {
  def apply(p1: Int, p2: Pinky) = println("Blinky.apply(" + p1 + ")")
}
