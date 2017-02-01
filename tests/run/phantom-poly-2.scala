/* Run this test with
 *   `run tests/run/phantom.scala -Xprint-diff-del -Xprint:arrayConstructors,phantomRefErasure,phantomDeclErasure,erasure`
 * to see the the diffs after PhantomRefErasure, PhantomDeclErasure and Erasure.
 */
import dotty.phantom.PhantomAny

object Test {

  def main(args: Array[String]): Unit = {
    polyfun2(new BlinkyImpl)
    polyfun2(new InkyImpl)
    polyfun2(new Pinky)
    polyfun2(Casper)
  }

  def polyfun2[P <: PhantomAny](p: P): Unit = {
    println("polyfun2")
  }
}

trait Blinky extends PhantomAny
abstract class Inky extends Blinky
class Pinky extends Inky
object Casper extends Pinky
class BlinkyImpl extends Blinky
class InkyImpl extends Inky
