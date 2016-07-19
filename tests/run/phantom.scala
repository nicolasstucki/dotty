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
    object Casper extends Pinky
  }

  def main(args: Array[String]): Unit = {
    class BlinkyImpl extends Blinky
    class InkyImpl extends Inky

    fun1(new BlinkyImpl)
    fun1(new InkyImpl)
    fun1(new Pinky)
    fun1(Casper)

    fun3(new BlinkyImpl, new Pinky)
    fun3(new InkyImpl, new Pinky)
    fun3(new Pinky, Casper)

    fun4(3, 4)(new BlinkyImpl, new Pinky)
    fun4(5, 6)(new InkyImpl, new Pinky)
    fun4(7, 8)(new Pinky, Casper)

    fun5(new BlinkyImpl, new Pinky)(15, 16)
    fun5(new InkyImpl, new Pinky)(17, 18)
    fun5(new Pinky, Casper)(19, 20)

    polyfun1()

    polyfun2(new Boo6().boo)
    polyfun2(new BlinkyImpl)
    polyfun2(new InkyImpl)
    polyfun2(new Pinky)
    polyfun2(Casper)

    polyfun3(new Boo6().boo)
    polyfun3(new BlinkyImpl)
    polyfun3(new InkyImpl)
    polyfun3(new Pinky)
    polyfun3(Casper)

    polyfun4(new Boo6().boo)
    polyfun4(new BlinkyImpl)
    polyfun4(new InkyImpl)
    polyfun4(new Pinky)
    polyfun4(Casper)

    new Boo1[PhantomAny]().polyfun1(new BlinkyImpl)
    new Boo1[PhantomAny]().polyfun1(new InkyImpl)

    new Boo2().polyfun1(new BlinkyImpl)
    new Boo2().polyfun1(new InkyImpl)
    new Boo2().polyfun1(new Pinky)

    new Boo3(){
      type Boo = PhantomAny
    }.polyfun1(new Pinky)
    new Boo3(){
      type Boo = Blinky
    }.polyfun1(new BlinkyImpl)

    new Boo4(new BlinkyImpl)
    new Boo4(new InkyImpl)
    new Boo4(new Pinky)

    new Boo5[PhantomAny](new Pinky)
    new Boo5[Pinky](new Pinky)

    fun(phantomFun1())

    fun(phantomFun2(new BlinkyImpl))
    fun(phantomFun2(new InkyImpl))
    fun(phantomFun2(new Pinky))

    fun(phantomFun3(new BlinkyImpl))
    fun(phantomFun3(new InkyImpl))
    fun(phantomFun3(new Pinky))

    fun(phantomFun4(new BlinkyImpl))
    fun(phantomFun4(new InkyImpl))
    fun(phantomFun4(new Pinky))

    pacFun1(new BlinkyImpl)
    pacFun1(new InkyImpl)
    pacFun1(new Pinky)

    pacFun2(new Pinky)

    pacFun3(new Pinky)

    hkFun1(new BlinkyImpl)
    hkFun1(new InkyImpl)
    hkFun1(new Pinky)

    fun(hkFun2(new BlinkyImpl))
    fun(hkFun2(new InkyImpl))
    fun(hkFun2(new Pinky))
  }

  def fun(top: PhantomAny): Unit = ()

  def fun1(top: PhantomAny): Unit = {
    println("fun1")
  }

  def fun2(bottom: PhantomNothing): Unit = {
    println("fun2")
  }

  def fun3(top: PhantomAny, bottom: Inky): Unit = {
    println("fun2")
  }

  def fun4(n: Int, n2: Int)(top: PhantomAny, bottom: Pinky): Unit = {
    println("fun4")
  }

  def fun5(top: PhantomAny, bottom: Clyde)(n: Int, n2: Int): Unit = {
    println("fun5")
  }

  def polyfun0[X <: AnyRef](): Unit = {
    println("polyfun0")
  }

  def polyfun1[P <: PhantomAny](): Unit = {
    println("polyfun1")
  }

  def polyfun2[P <: PhantomAny](p: P): Unit = {
    println("polyfun2")
  }

  def polyfun3[P <: PhantomAny, Q <: P](q: Q): Unit = {
    println("polyfun3")
  }

  def polyfun4[P >: PhantomNothing](p: P): Unit = {
    println("polyfun4")
  }

  class Boo1[P <: PhantomAny] {
    println("Boo1")
    def polyfun1(p1: P): Unit = {
      println("Boo1.polyfun1")
    }
  }

  class Boo2 {
    println("Boo2")
    type Boo = PhantomAny
    def polyfun1(p2: Boo): Unit = {
      println("Boo2.polyfun1")
    }
  }

  trait Boo3 {
    println("Boo3")
    type Boo <: PhantomAny
    def polyfun1(p3: Boo): Unit = {
      println("Boo3.polyfun1")
    }
  }

  class Boo4(p4: PhantomAny) {
    println("Boo4")
  }

  class Boo5[P <: PhantomAny](p5: P) {
    println("Boo5")
  }

  class Boo6 {
    println("Boo6")
    def boo: Pinky = new Pinky
  }

  def phantomFun1(): Pinky = new Pinky
  def phantomFun2(p6: PhantomAny): PhantomAny = p6
  def phantomFun3[P <: PhantomAny](p7: P): PhantomAny = p7
  def phantomFun4[P <: PhantomAny](p8: P): P = p8

  def pacFun1(blinky: Blinky) = {
    println("customPhantomsFun1")
  }
  def pacFun2(pinky: Pinky) = {
    println("customPhantomsFun2")
  }
  def pacFun3(clyde: Clyde) = {
    println("customPhantomsFun3")
  }

  type HKPhantom[X <: PhantomAny] = X

  def hkFun1[Y <: PhantomAny](p9: HKPhantom[Y]) = {
    println("hkFun1")
  }

  def hkFun2[Y <: PhantomAny](p10: HKPhantom[Y]): HKPhantom[Y] = p10

}
