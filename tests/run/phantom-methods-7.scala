/* Run this test with
 *   `run tests/run/phantom.scala -Xprint-diff-del -Xprint:arrayConstructors,phantomRefErasure,phantomDeclErasure,erasure`
 * to see the the diffs after PhantomRefErasure, PhantomDeclErasure and Erasure.
 */
import dotty.phantom.PhantomAny

object Test extends AbstractTypes {

  def main(args: Array[String]): Unit = {
    pacFun3(new Pinky)
  }

  def pacFun3(clyde: Clyde) = {
    println("customPhantomsFun3")
  }

}

trait AbstractTypes {
  type Clyde >: Pinky <: Inky
}

abstract class Inky extends PhantomAny
class Pinky extends Inky
