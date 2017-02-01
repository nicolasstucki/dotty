/* Run this test with
 *   `run tests/run/phantom.scala -Xprint-diff-del -Xprint:arrayConstructors,phantomRefErasure,phantomDeclErasure,erasure`
 * to see the the diffs after PhantomRefErasure, PhantomDeclErasure and Erasure.
 */
import dotty.phantom.PhantomAny

object Test {
  import Phantoms._

  def main(args: Array[String]): Unit = {
    fun(phantomFun1())
  }

  def fun(top: PhantomAny): Unit = ()


}

object Phantoms extends PhantomAny {
  class Pinky extends PhantomAny

  def phantomFun1(): Pinky = new Pinky
}
