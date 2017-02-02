/* Run this test with
 *   `run tests/run/phantom.scala -Xprint-diff-del -Xprint:arrayConstructors,phantomRefErasure,phantomDeclErasure,erasure`
 * to see the the diffs after PhantomRefErasure, PhantomDeclErasure and Erasure.
 */
import dotty.phantom.PhantomAny

object Test extends AbstractTypes {

  def main(args: Array[String]): Unit = {
    fun5(new BlinkyImpl)(15)(new Pinky)(16)
    fun5(new InkyImpl)(17)(new Pinky)(18)
    fun5(new Pinky)(19)(Casper)(20)
  }

  def fun5(top: PhantomAny)(n: Int)(bottom: Clyde)(n2: Int): Unit = {
    println("fun5")
  }
}

trait AbstractTypes {
  type Clyde >: Pinky <: Inky
}

trait Blinky extends PhantomAny
class InkyImpl extends Inky
abstract class Inky extends Blinky
class Pinky extends Inky
class BlinkyImpl extends Blinky
object Casper extends Pinky
