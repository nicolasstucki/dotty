
object Test {
  def main(args: Array[String]): Unit = {
    checkTrace(new Bar().foo(1), List("foo", "throws"))
  }

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

trait Foo[T]()(implicit spec: Specialized[T]) {
  def foo(x: T) = Test.throws(x)
}

class Bar extends Foo[Int]()
