object Test {

  def main(args: Array[String]): Unit = {

    def !!! : Null = ???
    unused def &&& : Null = ???

    fun(&&&)
    try {
      fun(!!!)
    } catch {
      case e: NotImplementedError => println("OK")
    }
  }

  def fun(unused bottom: Null): Unit = {
    println("fun")
  }
}
