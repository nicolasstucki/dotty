/* Run this test with
 *   `run tests/run/phantom.scala -Xprint-diff-del -Xprint:arrayConstructors,phantomRefErasure,phantomDeclErasure,erasure`
 * to see the the diffs after PhantomRefErasure, PhantomDeclErasure and Erasure.
 */
object Test {
  import dotty.phantom.PhantomAny
  import dotty.phantom.PhantomNothing
  import Phantoms._

  object Phantoms {
    trait Blinky extends PhantomAny
    abstract class Inky extends Blinky
  }

  def main(args: Array[String]): Unit = {
    class BlinkyImpl extends Blinky
    class InkyImpl extends Inky

    new Boo1[PhantomAny]().polyfun1(new BlinkyImpl)
    new Boo1[PhantomAny]().polyfun1(new InkyImpl)
  }

  def fun(top: PhantomAny): Unit = ()

  class Boo1[P <: PhantomAny] {
    println("Boo1")
    def polyfun1(p1: P): Unit = {
      println("Boo1.polyfun1")
    }
  }
}
