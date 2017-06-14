object Test {
  def main(args: Array[String]): Unit = {
    checkTrace(foo1[Int](), List("foo1$spec", "throws"))
    checkTrace(foo2[Int](1), List("foo2$spec", "throws$spec"))
    checkTrace(foo3(1), List("foo3$spec", "throws$spec"))
    checkTrace(foo3("abc"), List("foo3", "throws"))
    checkTrace(foo4(1, 0.2), List("foo4$spec", "throws$spec"))
    checkTrace(foo5(1, 0.2), List("foo5$spec", "throws"))
    checkTrace(foo6(1, true), List("foo6$spec", "throws$spec"))
    checkTrace(foo7(1, true), List("foo7$spec", "foo7$spec", "throws$spec"))
    try { // non-determinism on the names of inner function mangling
      checkTrace(foo8(1), List("foo8$spec", "foo8_2$1", "throws$spec"))
    } catch {
      case _: AssertionError =>
        checkTrace(foo8(1), List("foo8$spec", "foo8_2$2", "throws$spec"))
    }
    checkTrace(foo9(1, false), List("foo9$spec", "throws$spec"))
    checkTrace(foo10(1), List("foo10$spec", "throws$spec"))
    checkTrace(foo10("abc"), List("foo10", "throws"))
    checkTrace(foo10(new Object), List("foo10", "throws$spec"))
    foo11(1)
    checkTrace(foo12(new VC(1)), List("foo12$spec$1", "throws$spec$1"))
    checkTrace(foo12(new VC2(1)), List("foo12$spec$2", "throws$spec$2"))
    checkTrace(foo12(1), List("foo12$spec", "throws$spec"))
    checkTrace(foo13(new A), List("foo13", "foo$spec", "throws$spec"))
    checkTrace(foo13(new B), List("foo13", "foo$spec", "throws2$spec")) // FIXME: B.foo13 not getting specialized
//    checkTrace(foo14(1, 2), List()) // FIXME: ambigouous overload
    checkTrace(foo15(1), List("foo15$spec", "foo15_inner$spec$1", "throws$spec"))
    checkTrace(foo16(1), List("foo16$spec", "foo16_inner$spec", "throws$spec"))
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

  def foo13[T](a: A) = {
    a.foo(1)
  }

  def foo14[T: Specialized](x: Int, y: T) = {
    throws(y)
  }
  def foo14[T: Specialized](x: T, y: Int) = {
    throws(x)
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

  def throws[U: Specialized](x: U): Unit = throw new StackCheck

  def throws2[U: Specialized](x: U): Unit = throw new StackCheck

  class StackCheck extends Throwable

  def checkTrace[Thunk](thunk: => Thunk, expectedTrace: List[String]): Unit = {
    try {
      thunk
    } catch {
      case sc: StackCheck =>
        val trace = sc.getStackTrace().toList.takeWhile(!_.getMethodName.startsWith("main")).map(_.getMethodName).reverse
        assert(trace == expectedTrace, s"expected $expectedTrace but was $trace")
    }
  }
}

class A {
  def foo[T: Specialized](x: T) = Test.throws(x)
}

class B extends A {
  override def foo[T: Specialized](x: T) = Test.throws2(x)
}
