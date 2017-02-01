/* Run this test with
 *   `run tests/run/phantom.scala -Xprint-diff-del -Xprint:arrayConstructors,phantomRefErasure,phantomDeclErasure,erasure`
 * to see the the diffs after PhantomRefErasure, PhantomDeclErasure and Erasure.
 */
import dotty.phantom.PhantomAny

object Test {

  def main(args: Array[String]): Unit = {
    polyfun4(new BlinkyImpl)
    polyfun4(new InkyImpl)
    polyfun4(new Pinky)
    polyfun4(Casper)
  }

  def polyfun4[P >: PhantomNothing](p: P): Unit = {
    println("polyfun4")
  }
}

trait Blinky extends PhantomAny
abstract class Inky extends Blinky
class Pinky extends Inky
class InkyImpl extends Inky
class BlinkyImpl extends Blinky
object Casper extends Pinky
