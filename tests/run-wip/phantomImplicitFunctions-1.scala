
object Test {

  type Ctx[T] = implicit CanPrint => T

  def contextualPrintln(s: String): Ctx[Unit] = implicit (canPrint: CanPrint) => println(s)

  def main(args: Array[String]) = {
    import CanPrint._
    contextualPrintln("abc")
  }
}

class CanPrint extends PhantomAny
object CanPrint extends PhantomAny {
  implicit def canPrint: CanPrint = new CanPrint
}
