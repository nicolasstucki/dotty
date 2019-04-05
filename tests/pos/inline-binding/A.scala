
object A {
  inline def f(a: String, b: => String, inline c: String): Unit = {
    println(a)
    println(a)
    println(b)
    println(b)
    println(c)
    println(c)
  }
}