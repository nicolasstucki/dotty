import dotty.phantom.PhantomAny

class Blinky extends PhantomAny

object Test {
  def main(args: Array[String]): Unit = {
    val anonPhantomsFunction1 = new PhantomsFunction1_0[Blinky, Unit] {
      def apply(p1: Blinky) = println("AnnonPhantomsFunction1.apply() 1")
    }
    anonPhantomsFunction1(new Blinky)

    (new PhantomsFunction1_0[Blinky, Unit] {
      def apply(p1: Blinky) = println("AnnonPhantomsFunction1.apply() 2")
    }).apply(new Blinky)

    (new PhantomsFunction2_01[Blinky, Int, Unit] {
      def apply(p1: Blinky, i: Int) = println("AnnonPhantomsFunction1.apply() " + i)
    }).apply(new Blinky, 3)

  }
}

