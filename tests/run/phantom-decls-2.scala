/* Run this test with
 *   `run tests/run/phantom.scala -Xprint-diff-del -Xprint:arrayConstructors,phantomRefErasure,phantomDeclErasure,erasure`
 * to see the the diffs after PhantomRefErasure, PhantomDeclErasure and Erasure.
 */
import dotty.phantom.PhantomAny

object Test {

  def main(args: Array[String]): Unit = {
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

trait Blinky extends PhantomAny
abstract class Inky extends Blinky
class BlinkyImpl extends Blinky
class InkyImpl extends Inky
class Pinky extends Inky
