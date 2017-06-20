
object Test {

  def main(args: Array[String]): Unit = {
    checkTrace(B.foo(2), List("foo$spec$1", "throws$spec$1"))
    checkTrace(foo13(new A), List("foo13", "bar$spec$1", "throws$spec$1"))
    checkTrace(foo13(new B), List("foo13", "bar$spec$1", "throws2$spec$1"))
  }

  def foo13(a: A) = {
    a.bar(1)
  }

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
