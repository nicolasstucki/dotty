import scala.annotation.internal

class Foo {
  @internal.link.AssertReachable
  def foo = System.out.println(42)
  val bar = foo
}

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 20, classesWithReachableMethods = 7, reachableMethods = 10)
  def main(args: Array[String]): Unit = {
    new Foo
  }
}
