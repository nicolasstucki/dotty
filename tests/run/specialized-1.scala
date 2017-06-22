object Test {
  def main(args: Array[String]): Unit = {
    val throwInt = "throws$spec$1"
    val throwInt2 = "throws$spec$2"
    val throw2Int = "throws2$spec$3"
    val throw2Long = "throws2$spec$5"
    val throwDouble = "throws$spec$7"
    val throwBoolean = "throws$spec$6"
    val throw2Boolean = "throws2$spec$2"
    val throwA = "throws$spec$1"
    val throwVC = "throws$spec$4"
    val throwVC2 = "throws$spec$5"
    checkTrace(foo1[Int](), List("foo1$spec$1", "throws"))
    checkTrace(foo2[Int](1), List("foo2$spec$1", throwInt))
    checkTrace(foo3(1), List("foo3$spec$1", throwInt2))
    checkTrace(foo3("abc"), List("foo3", "throws"))
    checkTrace(foo4(1, 0.2), List("foo4$spec$1", throwInt2))
    checkTrace(foo5(1, 0.2), List("foo5$spec$1", "throws"))
    checkTrace(foo6(1, true), List("foo6$spec$1", throwInt2))
    checkTrace(foo7(1, true), List("foo7$spec$1", "foo7$spec$1", throwInt2))
    checkTrace(foo8(1), List("foo8$spec$1", "foo8_2$2", throwInt2))
    checkTrace(foo9(1, false), List("foo9$spec$1", throwInt2))
    checkTrace(foo10(1), List("foo10$spec$1", throwInt))
    checkTrace(foo10("abc"), List("foo10", "throws"))
    checkTrace(foo10(new Object), List("foo10", throwInt2))
    foo11(1)
    checkTrace(foo12(new VC(1)), List("foo12$spec$1", throwVC))
    checkTrace(foo12(new VC2(1)), List("foo12$spec$2", throwVC2))
    checkTrace(foo12(1), List("foo12$spec$3", throwInt2))
    checkTrace(foo13(new A), List("foo13", "foo$spec$1", throwInt2))
    checkTrace(foo13(new B), List("foo13", "foo$spec$1", throw2Int))
    // checkTrace(foo14(true, 2), List("foo14$spec$1", "throws2$spec$1")) // FIXME: overload ambigouity
    // checkTrace(foo14(1, false), List("foo14$spec$1", throwInt))
    checkTrace(foo15(1), List("foo15$spec$1", "foo15_inner$spec$1$1", throwInt2))
    checkTrace(foo16(1), List("foo16$spec$1", "foo16_inner$spec$1", throwInt2))
    checkTrace(foo16(new A), List("foo16", "foo$spec$1", throwInt2))
    checkTrace(foo16(new B), List("foo16", "foo$spec$1", throw2Int))
    checkTrace(foo17_1(new B), List("foo17_1", "foo$spec$3", throw2Long))
    checkTrace(foo17_2(new A), List("foo17_2", "foo$spec$2", throwDouble))
    checkTrace(foo18[true](), List("foo18$spec$1", "throws"))
    checkTrace(foo18[false](), List("foo18$spec$1", "throws"))
    checkTrace(foo19[true](true), List("foo19$spec$1", throwBoolean))
    checkTrace(foo19[false](false), List("foo19$spec$1", throw2Boolean))
    val a = new A()
    checkTrace(a.foo(1), List("foo$spec$1", throwInt2))
  }

  def foo1[T: Specialized]() = {
    throws(null)
    ()
  }

  def foo2[T: Specialized](x: T) = {
    throws(x)
    ()
  }

  def foo3[T: Specialized](x: T): T = {
    throws(x)
    x
  }

  def foo4[T: Specialized, U](x: T, y: U) = {
    throws(x)
  }

  def foo5[T: Specialized, U](x: T, y: U) = {
    throws(y)
  }

  def foo6[T: Specialized](x: T, b: Boolean): Unit = {
    if (b) foo6(x, false) // tailrec and specialized
    else throws(x)
  }

  def foo7[T: Specialized](x: T, b: Boolean): Unit = {
    if (b) {
      foo7(x, false) // non-tailrec and specialized
      foo7(x, false)
    } else throws(x)
  }

  def foo8[T: Specialized](x: T): T = {
    def foo8_2() = throws(x)
    foo8_2()
    x
  }

  def foo9[T: Specialized](x: T, b: Boolean): T = {
    if (b) return x
    else throws(x)
    x
  }

  def foo10[T: Specialized](x: T): Unit = {
    x match {
      case x: Int => throws(x)
      case x: String => throws(x)
      case x => throws(x.hashCode)
    }
  }

  def foo11[T: Specialized](x: T): Unit = {
    assert(x.isInstanceOf[Int])
    assert(!x.isInstanceOf[String])
    assert(x.isInstanceOf[T])
    x.asInstanceOf[Int]
    x.asInstanceOf[T]
  }

  def foo12[T: Specialized](x: T): Unit = {
    throws(x)
  }

  class VC(x: Int) extends AnyVal {
    def foo(): Int = x
  }

  class VC2(x: Int) extends AnyVal {
    def bar(): Int = x
  }

  def foo13(a: A) = {
    a.foo(1)
  }

  def foo14[T: Specialized](x: Int, y: T) = {
    throws(y)
  }
  def foo14[T: Specialized](x: T, y: Int) = {
    throws2(x)
  }

  def foo15[T: Specialized](x: T) = {
    def foo15_inner[U: Specialized](y: U) = throws(y)
    foo15_inner(x)
  }

  def foo16[T: Specialized](x: T) = {
    class Foo16 {
      def foo16_inner[U: Specialized](y: U) = throws(y)
    }
    val y = new Foo16
    y.foo16_inner(x)
  }

  def foo16[X <: A](a: X) = {
    a.foo(1) // FIXME fails -Ycheck:all
  }

  def foo17_1(a: B) = {
    a.foo(1L)
  }
  def foo17_2(a: A) = {
    a.foo(1.0)
  }

  def foo18[T: Specialized]() = {
    throws(null)
    ()
  }

  def foo19[T <: Boolean : Specialized](x: T) = {
    if (x) throws(x)
    else throws2(x)
  }

  def throws[U: Specialized](x: U): Unit = throw new StackCheck

  def throws2[U: Specialized](x: U): Unit = throw new StackCheck

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

class A {
  def foo[T: Specialized](x: T) = Test.throws(x)
}

class B extends A {
  override def foo[T: Specialized](x: T) = Test.throws2(x)
}
