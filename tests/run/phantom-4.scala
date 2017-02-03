/* Run this test with
 *   `run tests/run/phantom.scala -Xprint-diff-del -Xprint:arrayConstructors,phantomRefErasure,phantomDeclErasure,erasure`
 * to see the the diffs after PhantomRefErasure, PhantomDeclErasure and Erasure.
 */

object Test {

  def main(args: Array[String]): Unit = {
    fun4(3, 4, new BlinkyImpl, new Pinky)
    fun4(5, 6, new InkyImpl, new Pinky)
    fun4(7, 8, new Pinky, Casper)
  }

  def fun4(n: Int, n2: Int, top: PhantomAny, bottom: Pinky): Unit = {
    println("fun4")
  }

}

trait Blinky extends PhantomAny
abstract class Inky extends Blinky
class Pinky extends Inky
class BlinkyImpl extends Blinky
class InkyImpl extends Inky
object Casper extends Pinky
