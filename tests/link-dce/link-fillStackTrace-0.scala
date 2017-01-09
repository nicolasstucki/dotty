import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 23, classesWithReachableMethods = 9, reachableMethods = 11)
  def main(args: Array[String]): Unit = {
    try {
      throw new ThrowableFoo
    } catch {
      case _: ThrowableFoo => System.out.println(42)
    }
  }
}

class ThrowableFoo extends Throwable {
  @internal.link.AssertReachable
  override def fillInStackTrace(): Throwable = this
}
