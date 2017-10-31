object Test {

  def main(args: Array[String]): Unit = {
    {
      println("Test0")
      Test
    }.foo0

    {
      println("Test1")
      Test
    }.foo1()

    {
      println("Test2")
      Test
    }.foo2[Int]

    {
      println("Test3")
      Test
    }.foo3[Int]()

    ()
  }

  unused def foo0: Int = 0
  unused def foo1(): Int = 1
  unused def foo2[T]: Int = 2
  unused def foo3[T](): Int = 3

}
