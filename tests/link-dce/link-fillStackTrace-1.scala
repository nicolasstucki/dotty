import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 24, classesWithReachableMethods = 10, reachableMethods = 12)
  def main(args: Array[String]): Unit = {
    try {
      throw new BreakControl
    } catch {
      case _: BreakControl => System.out.println(42)
    }
  }
}

class BreakControl extends NoStackTrace

trait NoStackTrace extends Throwable {
  override def fillInStackTrace(): Throwable = this
}
