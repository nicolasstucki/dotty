/* Run this test with
 *   `run tests/run/phantom.scala -Xprint-diff-del -Xprint:arrayConstructors,phantomRefErasure,phantomDeclErasure,erasure`
 * to see the the diffs after PhantomRefErasure, PhantomDeclErasure and Erasure.
 */
object Test {
  import dotty.phantom.PhantomAny
  import dotty.phantom.PhantomNothing
  import Phantoms._

  trait Phantoms {
    type Clyde >: Pinky <: Inky
  }

  object Phantoms extends Phantoms {
    trait Blinky extends PhantomAny
    abstract class Inky extends Blinky
    class Pinky extends Inky
    object Casper extends Pinky
  }

  def main(args: Array[String]): Unit = {
    class BlinkyImpl extends Blinky
    class InkyImpl extends Inky

    polyfun4(new Boo().boo)
    polyfun4(new BlinkyImpl)
    polyfun4(new InkyImpl)
    polyfun4(new Pinky)
    polyfun4(Casper)
  }

  def polyfun4[P >: PhantomNothing](p: P): Unit = {
    println("polyfun4")
  }

  class Boo {
    println("Boo")
    def boo: Pinky = new Pinky
  }
}
