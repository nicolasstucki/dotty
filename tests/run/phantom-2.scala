/* Run this test with
 *   `run tests/run/phantom.scala -Xprint-diff-del -Xprint:arrayConstructors,phantomRefErasure,phantomDeclErasure,erasure`
 * to see the the diffs after PhantomRefErasure, PhantomDeclErasure and Erasure.
 */
object Test {
  import dotty.phantom.PhantomNothing

  def main(args: Array[String]): Unit = {}

  def fun2(bottom: PhantomNothing): Unit = {
    println("fun2")
  }
}
