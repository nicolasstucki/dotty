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

    fun(phantomFun3(new BlinkyImpl))
    fun(phantomFun3(new InkyImpl))
    fun(phantomFun3(new Pinky))
  }

  def fun(top: PhantomAny): Unit = println("fun")

  def phantomFun3[P <: PhantomAny](p7: P): PhantomAny = p7

}
