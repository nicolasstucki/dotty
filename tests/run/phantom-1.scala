/* Run this test with
 *   `run tests/run/phantom.scala -Xprint-diff-del -Xprint:arrayConstructors,phantomRefErasure,phantomDeclErasure,erasure`
 * to see the the diffs after PhantomRefErasure, PhantomDeclErasure and Erasure.
 */

object Test {

  def main(args: Array[String]): Unit = {
    fun1(new BlinkyImpl)
    fun1(new InkyImpl)
    fun1(new Pinky)
    fun1(Casper)
  }

  def fun1(top: PhantomAny): Unit = {
    println("fun1")
  }
}

trait Blinky extends PhantomAny
abstract class Inky extends Blinky
class Pinky extends Inky
class BlinkyImpl extends Blinky
class InkyImpl extends Inky
object Casper extends Pinky
