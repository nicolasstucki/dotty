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

  def throws[U: Specialized](x: U): Unit = throw new StackCheck

  class StackCheck extends Throwable

  def checkTrace[T](thunk: => T, expectedTrace: List[String]): Unit = {
    try {
      thunk
    } catch {
      case sc: StackCheck =>
        val trace = sc.getStackTrace().toList.takeWhile(!_.getMethodName.startsWith("main")).map(_.getMethodName).reverse
        assert(trace == expectedTrace, s"expected $expectedTrace but was $trace")
    }
  }
}
