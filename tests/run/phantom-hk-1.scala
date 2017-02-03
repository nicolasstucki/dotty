/* Run this test with
 *   `run tests/run/phantom.scala -Xprint-diff-del -Xprint:arrayConstructors,phantomRefErasure,phantomDeclErasure,erasure`
 * to see the the diffs after PhantomRefErasure, PhantomDeclErasure and Erasure.
 */

object Test {

  def main(args: Array[String]): Unit = {
    hkFun1(new BlinkyImpl)
    hkFun1(new InkyImpl)
    hkFun1(new Pinky)
  }

  type HKPhantom[X <: PhantomAny] = X

  def hkFun1[Y <: PhantomAny](p9: HKPhantom[Y]) = {
    println("hkFun1")
  }

}

trait Blinky extends PhantomAny
abstract class Inky extends Blinky
class Pinky extends Inky
class BlinkyImpl extends Blinky
class InkyImpl extends Inky
