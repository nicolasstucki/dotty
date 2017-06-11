object Test {
  def main(args: Array[String]): Unit = {
    foo1[Int]()
    foo2[Int](1)
    foo3(1)
    foo3("abc")
    foo4(1, 0.2)
    foo5(1, true)
  }

  def foo1[T: Specialized]() = {
    println2("foo1")
    ()
  }

  def foo2[T: Specialized](x: T) = {
    println2("foo2")
    println2(x)
    ()
  }

  def foo3[T: Specialized](x: T): T = {
    println2("foo3")
    println2(x)
    x
  }

  def foo4[T: Specialized, U](x: T, y: U) = {
    println2("foo4")
  }

  def foo5[T: Specialized](x: T, b: Boolean): Unit = {
    println2("foo5")
    if (b)
      foo5(x, false) // tailrec and specialized
  }

  def println2[U: Specialized](x: U): Unit = println(x)

}
