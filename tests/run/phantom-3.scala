/* Run this test with
 *   `run tests/run/phantom.scala -Xprint-diff-del -Xprint:arrayConstructors,phantomRefErasure,phantomDeclErasure,erasure`
 * to see the the diffs after PhantomRefErasure, PhantomDeclErasure and Erasure.
 */
import dotty.phantom.PhantomAny

object Test {

  def main(args: Array[String]): Unit = {
    fun3(new BlinkyImpl, new Pinky)
    fun3(new InkyImpl, new Pinky)
    fun3(new Pinky, Casper)
  }

  def fun3(top: PhantomAny, bottom: Inky): Unit = {
    println("fun3")
  }
}

trait Blinky extends PhantomAny
abstract class Inky extends Blinky
class Pinky extends Inky
class BlinkyImpl extends Blinky
class InkyImpl extends Inky
object Casper extends Pinky
