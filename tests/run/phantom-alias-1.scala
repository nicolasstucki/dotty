object Test {
  import Boo._
  import BooAlias._

  def main(args: Array[String]): Unit = {
    fun1(any1)
    fun1(any2)
    fun1(any3)
    fun1(any4)

    fun2(any1)
    fun2(any2)
    fun2(any3)
    fun2(any4)

    fun3(any1)
    fun3(any2)
    fun3(any3)
    fun3(any4)
  }

  def fun1(boo: BooAny1): Unit = {
    println("fun1")
  }

  def fun2(boo: BooAny2): Unit = {
    println("fun2")
  }

  def fun3(boo: BooAny3): Unit = {
    println("fun3")
  }
}

object BooAlias {
  type BooAny3 = Boo.BooAny1
  def any3: BooAny3 = Boo.any1
  val any4: BooAny3 = Boo.any2
}

object Boo extends Phantom {
  type BooAny1 = this.Any
  type BooAny2 = Boo.Any
  def any1: BooAny1 = assume
  def any2: BooAny2 = assume
}
