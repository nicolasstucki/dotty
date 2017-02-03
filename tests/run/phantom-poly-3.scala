/* Run this test with
 *   `run tests/run/phantom.scala -Xprint-diff-del -Xprint:arrayConstructors,phantomRefErasure,phantomDeclErasure,erasure`
 * to see the the diffs after PhantomRefErasure, PhantomDeclErasure and Erasure.
 */

object Test {

  def main(args: Array[String]): Unit = {
    polyfun3(new BlinkyImpl)
    polyfun3(new InkyImpl)
    polyfun3(new Pinky)
    polyfun3(Casper)
  }

  def polyfun3[P <: PhantomAny, Q <: P](q: Q): Unit = {
    println("polyfun3")
  }
}

trait Blinky extends PhantomAny
abstract class Inky extends Blinky
class Pinky extends Inky
class BlinkyImpl extends Blinky
class InkyImpl extends Inky
object Casper extends Pinky
