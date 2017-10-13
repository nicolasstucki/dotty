object Test {

  def main(args: Array[String]): Unit = {
    fun({ println("x1"); boo })({ println("x2"); boo })
    fun({ println("x3"); boo2 })({ println("x4"); boo2 })

    new Fun({ println("y1"); boo })({ println("y2"); boo })

    (new Fun2().fun)({ println("z1"); boo })({ println("z2"); boo })
  }

  def fun(unused x1: Int)(unused x2: Int) = {
    println("fun")
  }

  class Fun(unused y1: Int)(unused y2: Int) {
    println("Fun")
  }

  class Fun2 {
    println("Fun2")
    def fun(unused z1: Int)(unused z2: Int) = {
      println("Fun2fun")
    }
  }

  unused def boo: Int = {
    println("boo")
    42
  }
  def boo2: Int = {
    println("boo2")
    42
  }

}
