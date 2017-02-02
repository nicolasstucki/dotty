import dotty.phantom.PhantomAny

object Test {

  type Ctx[T] = implicit CanPrint => T

  def contextualPrintln(s: String): Ctx[Unit] = implicit (canPrint: CanPrint) => println(s)

  def main(args: Array[String]) = {
    implicit def canPrint: CanPrint = new CanPrint
    contextualPrintln("abc")
  }
}

class CanPrint extends PhantomAny
