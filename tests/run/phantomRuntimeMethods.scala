/* Run this test with
 *   `run tests/run/phantomRuntimeMethods.scala -Xprint-diff-del -Xprint:arrayConstructors,phantomRefErasure,phantomDeclErasure,erasure`
 * to see the the diffs after PhantomRefErasure, PhantomDeclErasure and Erasure.
 *
 * This is an example of how to write methods that return non phantom (i.e. methods with runtime code) values in a phantom class.
 */

import dotty.phantom.PhantomAny

object Test {

  implicit class RuntimeBlinky(blinky: Blinky) {
    def printFooLine(): Unit = {
      println("foo")
    }
  }

  def main(args: Array[String]): Unit = {
    import Blinky._
    blinky.printFooLine()
  }
}

class Blinky extends PhantomAny

object Blinky extends PhantomAny {
  def blinky = new Blinky
}
