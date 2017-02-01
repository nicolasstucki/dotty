/* Run this test with
 *   `run tests/run/phantom.scala -Xprint-diff-del -Xprint:arrayConstructors,phantomRefErasure,phantomDeclErasure,erasure`
 * to see the the diffs after PhantomRefErasure, PhantomDeclErasure and Erasure.
 */
import dotty.phantom.PhantomAny

object Test {
  import Phantoms._

  def main(args: Array[String]): Unit = {
    fun(phantomFun2(new BlinkyImpl))
    fun(phantomFun2(new InkyImpl))
    fun(phantomFun2(new Pinky))
  }

  def fun(top: PhantomAny): Unit = println("fun")
}

object Phantoms extends PhantomAny {
  trait Blinky extends PhantomAny
  abstract class Inky extends Blinky
  class BlinkyImpl extends Blinky
  class InkyImpl extends Inky
  class Pinky extends Inky

  def phantomFun2(p6: PhantomAny): PhantomAny = p6
}
