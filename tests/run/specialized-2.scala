object Test {
  def main(args: Array[String]): Unit = {
    checkTrace(foo1(2), List("foo1$spec$1", "apply", "$anonfun$2", "throws$spec$1"))
    checkTrace(foo2((x: Int) => x, 2), List("foo2$spec$1", "throws$spec$1"))
    val a: Int => Int = foo3[Int]
    checkTrace(a(2), List("apply", "foo3$spec$1$$anonfun$1", "throws$spec$1"))
    foo2bob[Int]
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

  def foo2bob[T: Specialized]: Bob[T] = new Bob[T]

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

class Bob[T]
