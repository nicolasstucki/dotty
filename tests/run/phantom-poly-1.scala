/* Run this test with
 *   `run tests/run/phantom.scala -Xprint-diff-del -Xprint:arrayConstructors,phantomRefErasure,phantomDeclErasure,erasure`
 * to see the the diffs after PhantomRefErasure, PhantomDeclErasure and Erasure.
 */
import dotty.phantom.PhantomAny

object Test {

  def main(args: Array[String]): Unit = {
    polyfun1()
    polyfun1[Casper]()
  }

  def polyfun1[P <: PhantomAny](): Unit = {
    println("polyfun1")
  }

}

class Casper extends PhantomAny
