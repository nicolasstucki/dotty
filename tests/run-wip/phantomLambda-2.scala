
class Casper extends PhantomAny

object Test {
  def main(args: Array[String]) = {
    val foo = (b: Casper, i: Int) => i
    foo(new Casper, 43)

    bar1(new Casper, new Casper)
    bar2(new Casper, new Casper)
  }

  val bar1: (Casper, Casper) => Unit = (b, b1) => println("bar1")
  val bar2 = (b: Casper, b2: Casper) => println("bar2")
}
