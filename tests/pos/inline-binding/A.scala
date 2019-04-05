
object A {
  inline def f(a: String, b: => String): Unit = {
    println(a)
    println(a)
    println(b)
    println(b)
  }
}