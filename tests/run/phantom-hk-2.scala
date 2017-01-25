/* Run this test with
 *   `run tests/run/phantom.scala -Xprint-diff-del -Xprint:arrayConstructors,phantomRefErasure,phantomDeclErasure,erasure`
 * to see the the diffs after PhantomRefErasure, PhantomDeclErasure and Erasure.
 */
object Test {
  import dotty.phantom.PhantomAny
  import dotty.phantom.PhantomNothing
  import Phantoms._

  object Phantoms {
    trait Blinky extends PhantomAny
    abstract class Inky extends Blinky
    class Pinky extends Inky
  }

  def main(args: Array[String]): Unit = {
    class BlinkyImpl extends Blinky
    class InkyImpl extends Inky

    fun(hkFun2(new BlinkyImpl))
    fun(hkFun2(new InkyImpl))
    fun(hkFun2(new Pinky))
  }

  def fun(top: PhantomAny): Unit = println("hk2")

  type HKPhantom[X <: PhantomAny] = X

  def hkFun2[Y <: PhantomAny](p10: HKPhantom[Y]): HKPhantom[Y] = p10

}
