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

    hkFun1(new BlinkyImpl)
    hkFun1(new InkyImpl)
    hkFun1(new Pinky)
  }

  type HKPhantom[X <: PhantomAny] = X

  def hkFun1[Y <: PhantomAny](p9: HKPhantom[Y]) = {
    println("hkFun1")
  }

}
