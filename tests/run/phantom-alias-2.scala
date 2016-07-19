object Test {
  import Boo._
  import BooAlias._

  def main(args: Array[String]): Unit = {
    fun1(boo)
    fun2(boo)
  }

  def fun1(boo: SomeBoo): Unit = {
    println("fun1")
  }

  def fun2(boo: SomeBoo2): Unit = {
    println("fun2")
  }
}

object BooAlias {
  type SomeBoo2 <: Boo.SomeBoo
}

object Boo extends Phantom {
  type SomeBoo <: this.Any
  def boo[B <: SomeBoo]: B = assume
}
