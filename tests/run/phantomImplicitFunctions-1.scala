import dotty.phantom.PhantomAny

object Test {

  class CanPrint extends PhantomAny

  type Ctx[T] = implicit CanPrint => T

  def contextualPrintln(s: String): Ctx[Unit] = implicit (canPrint: CanPrint) => println(s)

  def main(args: Array[String]): Unit = {
    implicit def canPrint: CanPrint = new CanPrint
    contextualPrintln("abc")
  }
}
