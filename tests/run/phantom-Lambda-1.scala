
object Test {
  import Boo._

  def main(args: Array[String]) = {
    foo1(boo[Casper])
    foo2(boo[Casper])
  }

  def foo1: Casper => Unit = b => println("foo1")
  def foo2 = (b: Casper) => println("foo2")
}

object Boo extends Phantom {
  type Casper <: Boo.Any
  def boo[B <: Boo.Any]: B = assume[B]
}
