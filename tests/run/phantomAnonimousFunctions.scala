
class Blinky extends PhantomAny

object Test {
  def main(args: Array[String]): Unit = {
    val anonPhantomsFunction1 = new Function1[Blinky, Unit] {
      def apply(p1: Blinky) = println("AnnonPhantomsFunction1.apply() 1")
    }
    anonPhantomsFunction1(new Blinky)

    (new Function1[Blinky, Unit] {
      def apply(p1: Blinky) = println("AnnonPhantomsFunction1.apply() 2")
    }).apply(new Blinky)

    (new Function2[Blinky, Int, Unit] {
      def apply(p1: Blinky, i: Int) = println("AnnonPhantomsFunction1.apply() " + i)
    }).apply(new Blinky, 3)

  }
}

