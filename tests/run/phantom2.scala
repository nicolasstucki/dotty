/* Run this test with
 *   `run tests/run/phantom2.scala -Xprint-diff-del -Xprint:arrayConstructors,phantomRefErasure,phantomDeclErasure,erasure`
 * to see the the diffs after PhantomRefErasure, PhantomDeclErasure and Erasure.
 */
object Test {
  import dotty.phantom.PhantomAny
  import dotty.phantom.PhantomNothing


  trait Blinky extends PhantomAny
  abstract class Inky extends Blinky
  class Pinky extends Inky

  def main(args: Array[String]): Unit = {
    fun(new Blinky {})
    fun(new Inky {})
    fun(new Pinky {})
  }

  def fun(top: PhantomAny): Unit = {
    println("fun")
  }

}
