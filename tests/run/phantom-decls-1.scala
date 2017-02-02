/* Run this test with
 *   `run tests/run/phantom.scala -Xprint-diff-del -Xprint:arrayConstructors,phantomRefErasure,phantomDeclErasure,erasure`
 * to see the the diffs after PhantomRefErasure, PhantomDeclErasure and Erasure.
 */
import dotty.phantom.PhantomAny

object Test {

  def main(args: Array[String]): Unit = {
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

trait Blinky extends PhantomAny
abstract class Inky extends Blinky
class BlinkyImpl extends Blinky
class InkyImpl extends Inky
