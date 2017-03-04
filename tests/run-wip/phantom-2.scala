/* Run this test with
 *   `run tests/run/xyz.scala -Xprint-diff-del -Xprint:arrayConstructors,phantomTermErasure,phantomTypeErasure,erasure`
 * to see the the diffs after PhantomRefErasure, PhantomDeclErasure and Erasure.
 */
object Test {
  import Boo._

  def main(args: Array[String]): Unit = {
    fun2(Boo.nothig)
  }

  def fun2(bottom: BooNothing): Unit = {
    println("fun2")
  }
}

object Boo extends Phantom {
  type BooNothing = this.Nothing
  def nothig: BooNothing = assume[BooNothing] // Should be allowed?
}
