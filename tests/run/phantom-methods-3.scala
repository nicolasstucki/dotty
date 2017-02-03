/* Run this test with
 *   `run tests/run/phantom.scala -Xprint-diff-del -Xprint:arrayConstructors,phantomRefErasure,phantomDeclErasure,erasure`
 * to see the the diffs after PhantomRefErasure, PhantomDeclErasure and Erasure.
 */

object Test {
  import Phantoms._

  def main(args: Array[String]): Unit = {
    fun(phantomFun3(new BlinkyImpl))
    fun(phantomFun3(new InkyImpl))
    fun(phantomFun3(new Pinky))
  }

  def fun(top: PhantomAny): Unit = println("fun")
}

trait Blinky extends PhantomAny
abstract class Inky extends Blinky
class Pinky extends Inky
class BlinkyImpl extends Blinky
class InkyImpl extends Inky

object Phantoms extends PhantomAny {
  def phantomFun3[P <: PhantomAny](p7: P): PhantomAny = p7
}
