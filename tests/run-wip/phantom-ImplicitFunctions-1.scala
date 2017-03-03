

object Test {
  import Boo._

  type Ctx[T] = implicit CanPrint => T

  def contextualPrintln(s: String): Ctx[Unit] = implicit (canPrint: CanPrint) => println(s)

  def main(args: Array[String]) = {
    implicit def canPrint: CanPrint = Boo.boo[CanPrint]
    contextualPrintln("abc")
  }
}

object Boo extends Phantom {
  type CanPrint <: this.Any
  def boo: CanPrint = assume
}
