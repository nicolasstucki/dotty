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
  }

  def main(args: Array[String]): Unit = {
    class BlinkyImpl extends Blinky
    class InkyImpl extends Inky

    new Boo2().polyfun1(new BlinkyImpl)
    new Boo2().polyfun1(new InkyImpl)
    new Boo2().polyfun1(new Pinky)
  }

  class Boo2 {
    println("Boo2")
    type Boo = PhantomAny
    def polyfun1(p2: Boo): Unit = {
      println("Boo2.polyfun1")
    }
  }
}
