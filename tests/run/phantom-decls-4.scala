/* Run this test with
 *   `run tests/run/phantom.scala -Xprint-diff-del -Xprint:arrayConstructors,phantomRefErasure,phantomDeclErasure,erasure`
 * to see the the diffs after PhantomRefErasure, PhantomDeclErasure and Erasure.
 */

object Test {

  def main(args: Array[String]): Unit = {
    new Boo4(new BlinkyImpl)
    new Boo4(new InkyImpl)
    new Boo4(new Pinky)
  }

  class Boo4(p4: PhantomAny) {
    println("Boo4")
  }
}
trait Blinky extends PhantomAny
abstract class Inky extends Blinky
class BlinkyImpl extends Blinky
class InkyImpl extends Inky
class Pinky extends Inky
