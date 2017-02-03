/* Run this test with
 *   `run tests/run/phantom.scala -Xprint-diff-del -Xprint:arrayConstructors,phantomRefErasure,phantomDeclErasure,erasure`
 * to see the the diffs after PhantomRefErasure, PhantomDeclErasure and Erasure.
 */

object Test {
  import Phantoms._

  def main(args: Array[String]): Unit = {

    fun(phantomFun4(new BlinkyImpl))
    fun(phantomFun4(new InkyImpl))
    fun(phantomFun4(new Pinky))
  }

  def fun(top: PhantomAny): Unit = println("fun")
}

object Phantoms extends PhantomAny {
  trait Blinky extends PhantomAny
  abstract class Inky extends Blinky
  class BlinkyImpl extends Blinky
  class InkyImpl extends Inky
  class Pinky extends Inky

  def phantomFun4[P <: PhantomAny](p8: P): P = p8
}
