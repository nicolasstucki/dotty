
object Test {
  import Boo._

  def main(args: Array[String]) = {
    val foo = (b: Casper, i: Int) => i
    foo(Boo.boo[Casper], 43)

    bar1(Boo.boo[Casper], Boo.boo[Casper])
    bar2(Boo.boo[Casper], Boo.boo[Casper])
  }

  val bar1: (Casper, Casper) => Unit = (b, b1) => println("bar1")
  val bar2 = (b: Casper, b2: Casper) => println("bar2")
}

object Boo extends Phantom {
  type Casper <: Boo.Any
  def boo[B <: Boo.Any]: B = assume[B]
}
