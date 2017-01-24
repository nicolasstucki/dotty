import dotty.phantom._

class Casper extends PhantomAny

object Test {
  def main(args: Array[String]) = {
    foo1(new Casper)
    foo2(new Casper)
  }

  def foo1: Casper => Unit = b => println("foo1")
  def foo2 = (b: Casper) => println("foo2")
}
