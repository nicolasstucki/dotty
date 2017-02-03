/* Run this test with
 *   `run tests/run/phantom.scala -Xprint-diff-del -Xprint:arrayConstructors,phantomRefErasure,phantomDeclErasure,erasure`
 * to see the the diffs after PhantomRefErasure, PhantomDeclErasure and Erasure.
 */

object Test {

  def main(args: Array[String]): Unit = {
    pacFun1(new BlinkyImpl)
    pacFun1(new InkyImpl)
    pacFun1(new Pinky)
  }

  def pacFun1(blinky: Blinky) = {
    println("customPhantomsFun1")
  }

}

trait Blinky extends PhantomAny
abstract class Inky extends Blinky
class Pinky extends Inky
class BlinkyImpl extends Blinky
class InkyImpl extends Inky
