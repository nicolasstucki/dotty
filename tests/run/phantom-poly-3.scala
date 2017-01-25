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

    polyfun3(new Boo().boo)
    polyfun3(new BlinkyImpl)
    polyfun3(new InkyImpl)
    polyfun3(new Pinky)
    polyfun3(Casper)
  }

  def polyfun3[P <: PhantomAny, Q <: P](q: Q): Unit = {
    println("polyfun3")
  }

  class Boo {
    println("Boo")
    def boo: Pinky = new Pinky
  }

}
