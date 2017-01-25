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
    object Casper extends Pinky
  }

  def main(args: Array[String]): Unit = {
    class BlinkyImpl extends Blinky
    class InkyImpl extends Inky

    fun(phantomFun2(new BlinkyImpl))
    fun(phantomFun2(new InkyImpl))
    fun(phantomFun2(new Pinky))

  }

  def fun(top: PhantomAny): Unit = println("fun")

  def phantomFun2(p6: PhantomAny): PhantomAny = p6

}
