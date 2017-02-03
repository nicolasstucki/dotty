/* Run this test with
 *   `run tests/run/phantom.scala -Xprint-diff-del -Xprint:arrayConstructors,phantomRefErasure,phantomDeclErasure,erasure`
 * to see the the diffs after PhantomRefErasure, PhantomDeclErasure and Erasure.
 */

object Test {

  def main(args: Array[String]): Unit = {

    new Boo3(){
      type Boo = PhantomAny
    }.polyfun1(new Pinky)
    new Boo3(){
      type Boo = Blinky
    }.polyfun1(new BlinkyImpl)
  }

  trait Boo3 {
    println("Boo3")
    type Boo <: PhantomAny
    def polyfun1(p3: Boo): Unit = {
      println("Boo3.polyfun1")
    }
  }
}

trait Blinky extends PhantomAny
abstract class Inky extends Blinky
class Pinky extends Inky
class BlinkyImpl extends Blinky
