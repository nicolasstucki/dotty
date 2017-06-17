object Test {
  def main(args: Array[String]): Unit = {
    checkTrace(foo1(2), List("foo1$spec$1", "apply", "$anonfun$2", "throws$spec$1"))
    checkTrace(foo2((x: Int) => x, 2), List("foo2$spec$1", "throws$spec$2"))
    val a: Int => Int = foo3[Int]
    checkTrace(a(2), List("apply", "foo3$spec$1$$anonfun$1", "throws$spec$2"))
    foo2bob[Int]
    foo2bob2[Int]
    foo4[Int]
    new CONS[Int](null).prepend[Int]
    NIL2.prepend(1)
    // new C(new L(1)).map(x => x) // FIXME
    //  SuccZipWith[Boolean](new ZipWith { type T = Int }) // FIXME
  }

  def foo1[T: Specialized](x: T) = {
    val fun = (x: T) => throws(x)
    fun(x)
  }

  def foo2[T: Specialized](x: T => T, y: T) = {
    throws(x(y))
  }

  def foo3[T: Specialized]: T => T = {
    (x: T) => { throws(x); x }
  }

  def foo4[T: Specialized] = this

  def foo2bob[T: Specialized]: Bob = new Bob

  def foo2bob2[T: Specialized]: Bob2[T] = new Bob2[T]

  def SuccZipWith[R](zWith : ZipWith): ZipWith { type T = zWith.T } =
    new ZipWith { type T = zWith.T }

  def throws[U: Specialized](x: U): Unit = throw new StackCheck

  class StackCheck extends Throwable

  def checkTrace[Thunk](thunk: => Thunk, expectedTrace: List[String]): Unit = {
    try {
      thunk
    } catch {
      case sc: StackCheck =>
        val trace = sc.getStackTrace().toList.takeWhile(!_.getMethodName.startsWith("main")).map(_.getMethodName).reverse
        val traceFmt = trace.map(x => "\"" + x + "\"")
        val expectedTraceFmt = expectedTrace.map(x => "\"" + x + "\"")
        assert(trace == expectedTrace, s"expected $expectedTraceFmt but was $traceFmt")
    }
  }
}

class Bob

class Bob2[T]

class CONS[+T](tl: CONS[T]) {
  def prepend[U >: T : Specialized]: CONS[U] = this
}

abstract class LIST2[+T] {
  def prepend [U >: T : Specialized] (x: U): LIST2[U] = new CONS2(x, this)
}

object NIL2 extends LIST2[Nothing]

class CONS2[U](hd: U, tl: LIST2[U]) extends LIST2[U]

class L[T](x: T)

class C[T](xs: L[T]) {
  def map[U](f: T => U): C[T] = new C(xs)
}

class ZipWith { type T }
