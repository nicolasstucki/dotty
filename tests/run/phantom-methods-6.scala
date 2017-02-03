/* Run this test with
 *   `run tests/run/phantom.scala -Xprint-diff-del -Xprint:arrayConstructors,phantomRefErasure,phantomDeclErasure,erasure`
 * to see the the diffs after PhantomRefErasure, PhantomDeclErasure and Erasure.
 */

object Test {

  def main(args: Array[String]): Unit = {
    pacFun2(new Pinky)
  }

  def pacFun2(pinky: Pinky) = {
    println("customPhantomsFun2")
  }

}

trait Blinky extends PhantomAny
abstract class Inky extends Blinky
class Pinky extends Inky
