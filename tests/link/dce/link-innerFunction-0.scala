import scala.annotation.internal

object Test {
  def main(args: Array[String]): Unit = {
    val bar = new Bar
    bar.foo1(42)
  }
}

class Bar extends Baz[Int]

class Baz[A] {

  @internal.link.AssertReachable def foo1(x: A): Unit = {
    @internal.link.AssertReachable def innerFoo(x: A): Unit = foo2(x)
    innerFoo(x)
  }

  @internal.link.AssertReachable def foo2(elem: A): Unit = {
    System.out.println(elem.toString)
  }

}
