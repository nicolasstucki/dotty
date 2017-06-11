object Test {
  def main(args: Array[String]): Unit = {
    foo1[Int]()
    foo2[Int](1)
    foo3(1)
    foo3("abc")
    foo4(1, 0.2)
  }

  def foo1[T: Specialized]() = {
    println("foo1")
    ()
  }

  def foo2[T: Specialized](x: T) = {
    println("foo2")
    println(x)
    ()
  }

  def foo3[T: Specialized](x: T): T = {
    println("foo3")
    println(x)
    x
  }

  def foo4[T: Specialized, U](x: T, y: U) = {
    println("foo4")
  }

  def println[U: Specialized](x: U): Unit = {
    scala.Predef.println(x)
  }
}
