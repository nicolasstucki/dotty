/* Run this test with
 *   `run tests/run/phantom.scala -Xprint-diff-del -Xprint:arrayConstructors,phantomRefErasure,phantomDeclErasure,erasure`
 * to see the the diffs after PhantomRefErasure, PhantomDeclErasure and Erasure.
 */

object Test {
  import Phantoms._

  def main(args: Array[String]): Unit = {
    class BlinkyImpl extends Blinky
    class InkyImpl extends Inky

    fun(hkFun2(new BlinkyImpl))
    fun(hkFun2(new InkyImpl))
    fun(hkFun2(new Pinky))
  }

  def fun(top: PhantomAny): Unit = println("hk2")

}

object Phantoms extends PhantomAny {
  trait Blinky extends PhantomAny
  abstract class Inky extends Blinky
  class Pinky extends Inky

  type HKPhantom[X <: PhantomAny] = X

  def hkFun2[Y <: PhantomAny](p10: HKPhantom[Y]): HKPhantom[Y] = p10
}
