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

    polyfun2(new Boo().boo)
    polyfun2(new BlinkyImpl)
    polyfun2(new InkyImpl)
    polyfun2(new Pinky)
    polyfun2(Casper)
  }

  def polyfun2[P <: PhantomAny](p: P): Unit = {
    println("polyfun2")
  }

  class Boo {
    println("Boo")
    def boo: Pinky = new Pinky
  }
}
