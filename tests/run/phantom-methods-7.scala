/* Run this test with
 *   `run tests/run/phantom.scala -Xprint-diff-del -Xprint:arrayConstructors,phantomRefErasure,phantomDeclErasure,erasure`
 * to see the the diffs after PhantomRefErasure, PhantomDeclErasure and Erasure.
 */
object Test {
  import dotty.phantom.PhantomAny
  import dotty.phantom.PhantomNothing
  import Phantoms._

  trait Phantoms {
    type Clyde >: Pinky <: Inky
  }

  object Phantoms extends Phantoms {
    trait Blinky extends PhantomAny
    abstract class Inky extends Blinky
    class Pinky extends Inky
  }

  def main(args: Array[String]): Unit = {
    pacFun3(new Pinky)
  }

  def pacFun3(clyde: Clyde) = {
    println("customPhantomsFun3")
  }

}
